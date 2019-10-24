namespace Orchid.Expressions

open System
open System.Globalization

open Orchid.Expressions

[<AutoOpen>]
module Scanner =

    /// Returns an IScanner to scan the given string
    [<CompiledName("StringScanner")>]
    let stringScanner (str:string) =
        let pos = ref -1
        let len = str.Length
        { 
            new IScanner with
                member x.HasNext() = 
                    str.Length > (!pos + 1)
                member x.Peek() = 
                    if x.HasNext() then Some(str.[!pos + 1]) else None
                member x.Current = 
                    if len > !pos then Some(str.[!pos]) else None
                member x.Advance() = 
                    pos := (!pos + 1)
                    str.Length >= !pos
                member x.Rewind() =
                    pos := !pos - 1 }

    /// Scanner extensions
    type IScanner with
        member x.TakeWhile (f: char -> bool) =
            let rec acc ls =
                let curr = x.Current
                if curr.IsSome && f(curr.Value) then
                    let concat = (ls @ [curr.Value])
                    x.Advance() |> ignore
                    acc concat
                else
                    ls

            acc []

module private Tokens =

    (* Token type test functions *)

    let isLParen   (token:Token) = match token with | LParen(_)      -> true | _ -> false
    let isRParen   (token:Token) = match token with | RParen(_)      -> true | _ -> false
    let isLCurly   (token:Token) = match token with | LCurly(_)      -> true | _ -> false
    let isRCurly   (token:Token) = match token with | RCurly(_)      -> true | _ -> false
    let isLSquare  (token:Token) = match token with | LSquare(_)     -> true | _ -> false
    let isRSquare  (token:Token) = match token with | RSquare(_)     -> true | _ -> false
    let isComma    (token:Token) = match token with | Comma(_)       -> true | _ -> false
    let isOperator (token:Token) = match token with | Operator(_, _) -> true | _ -> false
    let isSemiColon(token:Token) = match token with | SemiColon(_)   -> true | _ -> false
    let isEof      (token:Token) = match token with | EOF(_)         -> true | _ -> false

    let isTerminator (token:Token) =
        match token with
        | Comma(_) | RParen(_) | RCurly(_) | RSquare(_) | EOF(_) -> true
        | _ -> false

/// Module to define the precedence of each token and operator
module private Precedence =

    /// Returns the precedence of the given operator
    let ofOp (op:Op) =
        match op with
        | Op.Pow                                -> 0x7F
        | Op.Div  | Op.Mul   | Op.Mod           -> 0x7E
        | Op.Plus | Op.Minus                    -> 0x7D
        | Op.Lt   | Op.Gt    | Op.Lte | Op.Gte  -> 0x7C
        | Op.Eq   | Op.NEq                      -> 0x02
        | Op.And  | Op.Or                       -> 0x01
        | _                                     -> 0x00

    /// Returns the precedence of the given token
    let ofToken (token: Token) =
        match token with
        | Token.Operator(op, _) -> ofOp op
        | Token.LSquare(_)      -> 0x90
        | Token.LParen(_)       -> 0x80
        | _ -> 0

module Lexer = 
    
    [<Literal>]
    let LetBinding = "let"

    /// Allowed list of identifier characters
    let private IdentifierChars = 
        Set.ofList (['a' .. 'z'] @ ['A' .. 'Z'] @ ['.'; ':'; '_'] @ ['0' .. '9']);

    /// A map of operator chars to their corresponding enum values
    let private OpMap = dict ['/', Op.Div; 
                              '=', Op.Eq; 
                              '>', Op.Gt; 
                              '<', Op.Lt; 
                              '-', Op.Minus; 
                              '%', Op.Mod; 
                              '*', Op.Mul; 
                              '+', Op.Plus; 
                              '^', Op.Pow]

    /// A map of operator chars to their corresponding enum values
    let private CompositeOpMap = dict [">=", Op.Gte; 
                                       "<=", Op.Lte; 
                                       "<>", Op.NEq; 
                                       "!=", Op.NEq;
                                       "&&", Op.And;
                                       "||", Op.Or]

    /// Active pattern to test for a whitespace character
    let private (|Whitespace|_|) (ch:char option) = 
        match ch with
        | Some(ch) when ch = ' ' || ch = '\t' -> Some(ch)
        | _ -> None

    /// Active pattern to match the beginning of a string literal, the result is an option
    /// containing the QuoteType
    let private (|StringLiteral|_|) (ch:char option) =
        match ch with
        | Some(ch) when ch = '\"' -> Some(QuoteType.Double)
        | Some(ch) when ch = '\'' -> Some(QuoteType.Single)
        | _ -> None

    /// Active pattern testing for a composite operator
    let private (|CompositeOp|_|) (ch:char option, next:char option) = 
        if next.IsNone || ch.IsNone then 
            None 
        else
            let concatStr = (string ch.Value) + (string next.Value)
            if CompositeOpMap.ContainsKey(concatStr) then 
                Some(CompositeOpMap.[concatStr])
            else 
                None

    /// Active pattern testing for a single character operator
    let private (|OP|_|) (ch:char option) = 
        if ch.IsNone then 
            None
        elif OpMap.ContainsKey(ch.Value) then
            Some(OpMap.[ch.Value]) 
        else
            None

    /// Active pattern testing for a bracket type
    let private (|Bracket|_|) line col (ch:char option) =
        match ch with
        | Some('[') -> Some(LSquare ({Line = line; Column = col; Range = 1}))
        | Some(']') -> Some(RSquare ({Line = line; Column = col; Range = 1}))
        | Some('{') -> Some(LCurly  ({Line = line; Column = col; Range = 1}))
        | Some('}') -> Some(RCurly  ({Line = line; Column = col; Range = 1}))
        | Some('(') -> Some(LParen  ({Line = line; Column = col; Range = 1}))
        | Some(')') -> Some(RParen  ({Line = line; Column = col; Range = 1}))
        | _   -> None

    /// Converts a list of chars into a string
    let private charListToString (chars:char list) =
        let arr = chars |> Array.ofList
        new System.String(arr)

    /// Pushes the token onto the stack
    let inline private push (stack: Token list) (t:Token) = stack @ [t]

    /// String comparison, ignoring case
    let inline private strCompI str1 str2 = String.Compare(str1, str2, StringComparison.InvariantCultureIgnoreCase) = 0

    /// Converts a string to a double using an invariant culture
    let inline private stringToDouble (str: string) = 
        Double.Parse(str, NumberStyles.AllowDecimalPoint ||| NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture)

    /// Reads a number from the scanner.
    let private readNumber (scanner:IScanner) =
        let nums = scanner.TakeWhile Char.IsDigit
        let res = 
            if nums.Length > 0 && scanner.Current.IsSome && scanner.Current.Value = '.' then
                scanner.Advance() |> ignore
                nums @ ('.' :: (scanner.TakeWhile Char.IsDigit))                
            else nums
        let num = 
            res
            |> charListToString 
            |> stringToDouble
        scanner.Rewind() |> ignore
        num, res.Length

    /// Reads an identifier from the scanner
    let private readIdentifier (scanner:IScanner) =
        let res = 
            scanner.TakeWhile IdentifierChars.Contains 
            |> charListToString
        scanner.Rewind() |> ignore
        res

    [<Literal>] 
    let DoubleQuoteChar = '\"'
    
    [<Literal>] 
    let SingleQuoteChar = '\''

    /// This method constructs a list of tokens, using the given scanner implementation.
    [<CompiledName("Lex")>]
    let lex (scanner:IScanner) =
        let rec lexrec (line:int) (col:int) (stack: Token list) =
            if scanner.Advance() = false then stack else
            let next = scanner.Peek()
            match (scanner.Current, next) with
            | CompositeOp(op) -> 
                scanner.Advance() |> ignore
                Operator(op, {Line = line; Column = col; Range = 2})
                |> push stack
                |> (lexrec line (col + 2))
            | _ ->
                match scanner.Current with

                | Whitespace(ch) -> lexrec line (col + 1) stack

                | StringLiteral(quote) ->
                    scanner.Advance() |> ignore
                    let terminator = if quote = QuoteType.Double then DoubleQuoteChar else SingleQuoteChar
                    let str = (scanner.TakeWhile (fun ch -> ch <> terminator) |> charListToString)
                    (Token.String(str, quote, {Line = line; Column = col; Range = str.Length + 2}))
                    |> push stack 
                    |> (lexrec line (str.Length + col + 2))

                | Some(';') ->
                    SemiColon({Line = line; Column = col; Range = 1})
                    |> push stack
                    |> (lexrec line (col + 1))

                | Bracket line col t -> 
                    t 
                    |> push stack
                    |> (lexrec line (col + 1))

                | OP(op) -> 
                    Token.Operator(op, {Line = line; Column = col; Range = 1})
                    |> push stack
                    |> (lexrec line (col + 1))

                | Some(ch) when ch = '#' ->
                    let comment = 
                        scanner.TakeWhile (fun ch -> ch <> '\n' && ch <> '\r') 
                        |> charListToString
                    Token.Comment(comment, {Line = line; Column = col; Range = comment.Length})
                    |> push stack
                    |> (lexrec line (col + comment.Length + 1))

                | Some(ch) when ch = ',' -> 
                    Token.Comma({Line = line; Column = col; Range = 1}) 
                    |> push stack
                    |> (lexrec line (col + 1))

                | Some(x) when Char.IsDigit(x) -> 
                    let (num, numRead) = scanner |> readNumber
                    Token.Number(num, {Line = line; Column = col; Range = numRead})
                    |> push stack
                    |> (lexrec line (col + numRead))

                | Some(x) when IdentifierChars.Contains(x) -> 
                    let ident = (scanner |> readIdentifier)
                    let loc = {Line = line; Column = col; Range = ident.Length}
                    let token = 
                        if strCompI ident bool.TrueString then Token.Bool(true, loc) 
                        elif strCompI ident bool.FalseString then Token.Bool(false, loc) 
                        elif strCompI ident LetBinding then Token.Let(loc)
                        else Token.Identifier(ident, {Line = line; Column = col; Range = ident.Length})
                    token
                    |> push stack
                    |> (lexrec line (col + ident.Length))

                | Some('\r') -> 
                    if scanner.Peek() = Some('\n') then 
                        scanner.Advance() |> ignore
                    lexrec (line + 1) 0 stack

                | Some('\n') -> 
                    lexrec (line + 1) 0 stack
                
                | _ -> stack

        lexrec 1 0 []