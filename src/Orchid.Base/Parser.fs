namespace Orchid.Expressions

open System.Collections.Generic
open System.Text.RegularExpressions
open Orchid.Runtime

/// A private module used for validation of the expression + token list prior to the
/// result being returned to the caller.
module internal Validator =

    // Simple record type containing the bracket count and the token
    type private BI = {count: int; tok: Token}

    /// Active patter to match on literals
    let (|AnyLiteral|_|) (token:Token) = 
        match token with 
        | Token.Bool(_, _) | Token.Number(_, _) | Token.String(_, _, _) -> Some(true)
        | _ -> None

    /// Active pattern to match on no-ops
    let (|NoOps|_|) token = match token with | Comment(_, _) -> Some(true) | _ -> None

    /// Active pattern to match on open brackets
    let (|OpenBrackets|_|) token = 
        match token with 
        | LParen(_) | LCurly(_) | LSquare(_) -> Some(true)
        | _ -> None

    /// Active pattern to match on close brackets
    let (|CloseBrackets|_|) token = 
        match token with 
        | RParen(_) | RCurly(_) | RSquare(_) -> Some(true) 
        | _ -> None

    /// Active pattern to match on operators
    let (|Operators|_|) token = 
        match token with 
        | Token.Operator(_, _) -> Some(true) 
        | _ -> None

    let (|SemiColon|_|) token =
        match token with
        | Token.SemiColon(_) -> Some(true)
        | _ -> None

    /// Active pattern to match on unary operators
    let (|Unary|_|) token = 
        match token with 
        | Operator(op, _) when op = Op.Minus -> Some(true) 
        | _ -> None

    let (|LetBinding|_|) token =
        match token with
        | Token.Let(_) -> Some(true)
        | _ -> None

    /// Active pattern to match identifiers
    let (| Identifiers |_|) token =
        match token with | Token.Identifier(_, _) -> Some(true) | _ -> None

    /// Factory for creating parse errors for invalid token sequences
    let inline private mkError (first:Token) (next:Token) =
        ParseError(InvalidTokenSequence, (sprintf "Invalid sequence found: %A - %A" first next), first)

    /// A validator to iterate over the tokens looking for invalid token pairs
    let private invalidSequenceValidator (tokens:Token list) =
        
        let rec walk (head:Token) (tail: Token list) (acc: ParseError list) =

            if tail.IsEmpty then acc else
        
            let next = tail.Head

            match head with
            | Token.Let(tl) ->
                match next with
                | Identifiers(true) -> walk (tail.Head) (tail.Tail) acc
                | _ -> walk (tail.Head) (tail.Tail) ((mkError head next)::acc)

            | Token.Comma(tl) ->
                match next with
                | Identifiers(true) 
                | NoOps(true) 
                | OpenBrackets(true) 
                | AnyLiteral(true) 
                | Unary(true) -> walk (tail.Head) (tail.Tail) acc
                | _ -> walk (tail.Head) (tail.Tail) ((mkError head next)::acc)
            
            | Token.Identifier(_, _) ->
                match next with
                | NoOps(true) 
                | LParen(_) 
                | Comma(_)
                | LParen(_)
                | Operators(true)
                | SemiColon(_)
                | LetBinding(true)
                | CloseBrackets(true)-> walk (tail.Head) (tail.Tail) acc
                | _ -> walk (tail.Head) (tail.Tail) ((mkError head next)::acc)
            
            | Token.Operator(op, _) ->
                match next with
                | OpenBrackets(true) 
                | AnyLiteral(true) 
                | Identifiers(true) 
                | Unary(true) ->  walk (tail.Head) (tail.Tail) acc
                | _ -> walk (tail.Head) (tail.Tail) ((mkError head next)::acc)
            
            | CloseBrackets(true) ->
                match next with
                | NoOps(true)
                | Operators(true)
                | CloseBrackets(true)
                | Comma(_)
                | LetBinding(true)
                | SemiColon(_)
                | EOF(_) ->  walk (tail.Head) (tail.Tail) acc
                | _ -> walk (tail.Head) (tail.Tail) ((mkError head next)::acc)
            
            | OpenBrackets(true) ->
                match next with
                | Unary(true)
                | NoOps(true)
                | OpenBrackets(true)
                | AnyLiteral(true)
                | Identifiers(true)
                | LetBinding(true)
                | CloseBrackets(true) ->  walk (tail.Head) (tail.Tail) acc
                | _ -> walk (tail.Head) (tail.Tail) ((mkError head next)::acc)
            |_ -> walk (tail.Head) (tail.Tail) acc

        walk (tokens.Head) (tokens.Tail) []

    /// Bracket counter to ensure that all open brackets are closed
    let private bracketValidator (tokens:Token list) =
        
        /// iterate over all tokens incrementing and decrementing the bracket counts
        /// A final result <> 0 indicates an unbalanced bracket
        let rec validate (ls:Token list) (square:BI) (curly:BI) (round:BI): (BI * BI * BI) =
            match ls with
            | (LParen(tl)  as x)::xs -> validate xs square curly {count = round.count + 1; tok = x}
            | (RParen(tl)  as x)::xs -> validate xs square curly {count = round.count - 1; tok = x}
            | (LCurly(tl)  as x)::xs -> validate xs square {count = curly.count + 1; tok = x} round
            | (RCurly(tl)  as x)::xs -> validate xs square {count = curly.count - 1; tok = x} round
            | (LSquare(tl) as x)::xs -> validate xs {count = square.count + 1; tok = x} curly round
            | (RSquare(tl) as x)::xs -> validate xs {count = square.count - 1; tok = x} curly round
            | x::xs                  -> validate xs square curly round
            | _                      -> (square, curly, round)

        /// Helper to construct an EOF token to be used as a seed
        let inline mkSeed() = EOF({Line = -1; Column = -1; Range = 0 })

        let (square, curly, round) = 
            validate tokens {count = 0; tok = mkSeed()} {count = 0; tok = mkSeed()} {count = 0; tok = mkSeed()}

        // return a sequence containing ParseErrors for each of the failed bracket counts
        seq {
            if (square.count <> 0) then
                yield ParseError(UnmatchedSquareBracket, "Unmatched square bracket", square.tok)
            if (curly.count <> 0) then 
                yield ParseError(UnmatchedCurlyBracket, "Unmatched curly bracket", curly.tok)
            if (round.count <> 0) then
                yield ParseError(UnmatchedRoundBracket, "Unmatched round bracket",  round.tok)
        }
    
    /// Ensures local variable names are unique
    let validateUniqueAssignmentIdentifiers (expressions: Expr list) = 
        let ident expr =
            match expr with
            | Assign(Identifier(name,_), _, _) -> Some(name, expr)
            | _ -> None
        let lookup = HashSet<string>()
        seq {
            for ident, expr in (List.choose ident expressions) do
                if not (lookup.Add(ident)) then
                    yield (ParseError(DuplicateAssignment, sprintf "Duplicate local variable %s" ident, expr.Token))
        }

    let validateAssignmentIdentifiers (expressions: Expr list) = 
        // Identifiers must be valid on each assignment
        let isValidIdent (ident:string) = Regex.IsMatch(ident, "^[A-z]{1}[A-z0-9]*$")
        seq {
            for expr in expressions do
                match expr with
                | Assign(Identifier(name, _), _, _) ->
                    if not (isValidIdent name) then
                        yield (ParseError(InvalidAssignmentIdentifier, sprintf "%s is an invalid identifier" name, (expr.Token))) 
                | _ -> () }
                
    /// Validates a list of expressions to ensure that the it is structured correctly
    /// i.e. single statements must be a non assignment expression
    ///      multiple statements must contain assignments followed by a final expression (the result)
    let statementValidator (expressions: Expr list) =

        let isAssignment expr = 
            match expr with 
            | Assign(Identifier(name, _), right, _) -> true 
            | _ -> false
        
        // Returns a tuple of the number of assign expressions * number of non assign expressions
        let countAssignments arr =
            let assignments = Array.filter isAssignment arr
            assignments.Length, arr.Length - (assignments.Length)

        // Convert expression list to an array for easier retrieval of elements
        let exprArray = Array.ofList expressions

        // Count number of assignments
        let assignCount, exprCount = countAssignments exprArray

        if exprCount = 0 then
            Seq.singleton (ParseError(ExpressionHasNoReturnValue, 
                                      "Expression does not contain a return value", 
                                      (expressions.Head.Token)))
        elif exprCount > 1 then
            Seq.singleton (ParseError(ExpressionHasMultipleReturnValues, 
                                      "Expression can contain only a single return value", 
                                      (expressions.Head.Token)))
        elif assignCount > 0 && (isAssignment exprArray.[exprArray.Length - 1]) then
            Seq.singleton (ParseError(FinalExpressionMustBeResult, 
                                      "Final expression in statement must be the result", 
                                      (expressions.Head.Token)))
        else
            Seq.empty

    /// Walks the expression tree from the root to determine whether function identifiers are known to the system
    let private validateFunctions (expressions: Expr list)  (env: IEnvironment) =
        let rec validateList ls (env: IEnvironment) acc = List.iter (fun x -> validateInner x env acc) ls
        and validateInner inner (env: IEnvironment) (acc: ResizeArray<ParseError>) =
            match inner with
            | Expr.And(ls, _) -> validateList ls env acc
            | Expr.AnonymousFunc(ls, _) -> validateList ls env acc
            | Expr.Array(ls, _) -> validateList ls env acc
            | Expr.BinaryOperator(left, _, right, _) -> 
                validateInner left env acc
                validateInner right env acc
            | Expr.Bool(_, _)-> ()
            | Expr.Filter(expr, filter, _) ->  (validateInner expr env acc); (validateInner filter env acc)
            | Expr.ForEach(ls, _) -> validateList ls env acc
            | Expr.Function(Expr.Identifier(ident, token), ls, _) -> 
                if not (env.Functions.Exists(ident)) then
                    acc.Add(ParseError(UnknownFunction, (sprintf "Unknown function '%s'" ident), token)) |> ignore
            | Expr.IfThenElse(exp1, exp2, exp3, _) ->
                validateInner exp1 env acc
                validateInner exp2 env acc
                validateInner exp3 env acc
            | Expr.Macro(exp, _) -> validateInner exp env acc
            | Expr.Not(exp, _) -> validateInner exp env acc
            | Expr.Or(exprs, _) -> validateList exprs env acc
            | Expr.Parens(exp, _) -> validateInner exp env acc
            | Expr.UnaryOperator(_, exp, _) -> validateInner exp env acc
            | _ -> ()
        
        let errors = new ResizeArray<ParseError>()
        
        validateList expressions env errors   

        // upcast to a seq      
        errors :> seq<ParseError>        

    /// Performs validation on the initial token list
    let validateTokens (tokens:Token list) =
        tokens 
        |> bracketValidator
        |> Seq.append (invalidSequenceValidator (tokens))
        |> List.ofSeq

    /// Performs validation on the expression list
    let validateExpressions (expr: Expr list) (env: IEnvironment) =
        validateFunctions expr env
        |> Seq.append (validateAssignmentIdentifiers expr)
        |> Seq.append (statementValidator expr)        
        |> Seq.append (validateUniqueAssignmentIdentifiers expr)

/// The internal mutable version of the expression tree.  This is needed as some operations
/// require AST rewriting via ref cells which should only be supported during parsing
type private MutableExpr =
    | Assign            of MutableExpr * MutableExpr * Token
    | Array             of MutableExpr list * Token
    | Parens            of Ref<MutableExpr> * Token
    | Str               of string * QuoteType * Token
    | Bool              of bool * Token
    | Num               of double * Token
    | Identifier        of string * Token
    | Variable          of string * Token
    | BinaryOperator    of Ref<MutableExpr> * Op * Ref<MutableExpr> * Token
    | UnaryOperator     of Op * MutableExpr * Token
    | AnonymousFunc     of MutableExpr list * Token
    | Macro             of MutableExpr * Token
    | Function          of MutableExpr * MutableExpr list * Token
    | IfThenElse        of MutableExpr * MutableExpr * MutableExpr * Token
    | Filter            of MutableExpr * MutableExpr * Token
    | ForEach           of MutableExpr list * Token
    | Not               of MutableExpr * Token
    | Or                of MutableExpr list * Token
    | And               of MutableExpr list * Token
    | Invalid           of Token
    member x.Token =
        match x with
        | Array(_, t)                -> t
        | Assign(_, _, t)            -> t
        | Parens(_, t)               -> t
        | Str(_, _, t)               -> t
        | Bool(_, t)                 -> t
        | Num(_, t)                  -> t
        | Identifier(_, t)           -> t
        | Variable(_, t)             -> t
        | BinaryOperator(_, _, _, t) -> t
        | UnaryOperator(_, _, t)     -> t
        | Function(_, _, t)          -> t
        | IfThenElse(_, _, _, t)     -> t
        | Filter(_, _, t)            -> t
        | ForEach(_, t)              -> t
        | Not(_, t)                  -> t
        | Or(_, t)                   -> t
        | And(_, t)                  -> t
        | AnonymousFunc(_, t)        -> t
        | Macro(_, t)                -> t
        | Invalid(t)                 -> t

/// Represents the state held during a parse operation
type private IParseState =
    
    /// Adds a constructed error to the list
    abstract member AddError: ParseError -> unit

    /// Adds an error to the list of the form (code * message * token)
    abstract member AddError: int * string * Token -> unit

    /// The error list
    abstract member Errors: ParseError list with get

    /// Advances to the next token, returning false if there are no more tokens left on the stack
    abstract member Advance: unit -> bool

    // Returns the current token as an option
    abstract member Current: Token option with get

    // Returns the current list of tokens
    abstract member Tokens: Token list with get

    /// Returns the expression stack
    abstract member Stack: MutableExpr list with get

    /// Pops an expression off of the stack, returning the result as an option
    abstract member PopExpr: unit -> MutableExpr option

    /// Pushes a new expression onto the stack
    abstract member PushExpr: MutableExpr -> unit 

    /// Peeks at the expression at the head of the stack, returning the result as an Option<MutableExpr>
    abstract member PeekExpr: unit -> MutableExpr option

    /// Peeks at the token at the head of the stack, returning the result as an Option<Token>
    abstract member PeekToken: unit -> Token option

    /// Returns a list of prior tokens, enabling look backs
    abstract member PreviousTokens: unit -> Token list with get
    
module public Parser =

    /// This function transforms the MutableExpr hierarchy into a public Expr tree (without any mutable structures)    
    let rec private toPublicAst (stack:MutableExpr list) =
        
        /// Transforms a single MutableExpr into an Expr
        let rec toPublicExpr (mut:MutableExpr) =
            match mut with
            | MutableExpr.And(exprs, tok)               -> Expr.And((toPublicAst exprs), tok)
            | MutableExpr.AnonymousFunc(exprs, tok)     -> Expr.AnonymousFunc(toPublicAst(exprs), tok)
            | MutableExpr.Array(exprs, tok)             -> Expr.Array(toPublicAst(exprs), tok)
            | MutableExpr.Assign(ident, expr, tok)      -> Expr.Assign(toPublicExpr(ident), toPublicExpr(expr), tok)
            | MutableExpr.BinaryOperator(l, op, r, tok) -> Expr.BinaryOperator(toPublicExpr(!l), op, toPublicExpr(!r), tok)
            | MutableExpr.Bool(value, tok)              -> Expr.Bool(value, tok)
            | MutableExpr.Filter(expr1, expr2, tok)     -> Expr.Filter(toPublicExpr(expr1), toPublicExpr(expr2), tok)
            | MutableExpr.ForEach(exprs, tok)           -> Expr.ForEach(toPublicAst(exprs), tok)
            | MutableExpr.Function(expr1, exprs, tok)   -> Expr.Function(toPublicExpr(expr1), toPublicAst(exprs), tok)
            | MutableExpr.Identifier(ident, tok)        -> Expr.Identifier(ident, tok)
            | MutableExpr.IfThenElse(e1, e2, e3, tok)   -> Expr.IfThenElse(toPublicExpr(e1), 
                                                                           toPublicExpr(e2),
                                                                           toPublicExpr(e3), tok)
            | MutableExpr.Invalid(tok)                  -> Expr.Invalid(tok)
            | MutableExpr.Macro(expr, tok)              -> Expr.Macro(toPublicExpr(expr), tok)
            | MutableExpr.Not(expr, tok)                -> Expr.Not(toPublicExpr(expr), tok)
            | MutableExpr.Num(value, tok)               -> Expr.Num(value, tok)
            | MutableExpr.Or(exprs, tok)                -> Expr.Or(toPublicAst exprs, tok)
            | MutableExpr.Parens(exprs, tok)            -> Expr.Parens(toPublicExpr (!exprs), tok)
            | MutableExpr.Str(value, q, tok)            -> Expr.Str(value, q, tok)
            | MutableExpr.UnaryOperator(op, expr, tok)  -> Expr.UnaryOperator(op, toPublicExpr(expr), tok)
            | MutableExpr.Variable(value, tok)          -> Expr.Variable(value, tok)
            
        and walk (ls:MutableExpr list) (acc:Expr list) =
            match ls with
            | x::xs -> walk xs ((toPublicExpr(x)) :: acc)
            | _ -> acc

        // walk the tree - note that the root nodes must be reversed here
        walk stack [] |> List.rev

    /// A regular expression active pattern which returns true when the re matches the given strings
    /// and none when it does not
    let private (|RegExMatch|_|) (pattern:string) (s:string) = 
        if Regex.IsMatch(s, pattern)
        then Some(true) 
        else None

    /// An object expression used to construct an implementation of an IParseState
    let private createParseState (t: Token list) =

        let (current: Ref<Token option>) = ref None
        let (tokens:Ref<Token list>) = ref t
        let (stack: Ref<MutableExpr list>) = ref []
        let (prior: Ref<Token list>) = ref []
        let (errors: Ref<ParseError list>) = ref []
        { new IParseState with
            member x.AddError(err:ParseError) = errors := err :: !errors
            member x.AddError (code, msg, token) = x.AddError(ParseError(code, msg, token))
            member x.Errors with get() = !errors
            member x.Current with get() = !current
            member x.Tokens with get() = !tokens
            member x.Stack with get() = !stack
            member x.PushExpr(e:MutableExpr) = stack := (e::!stack)
            member x.PreviousTokens with get() = !prior
            member x.PopExpr() = 
                match x.Stack with
                | x::xs -> stack := xs; Some(x)
                | _ -> None
            member x.PeekToken() =
                match x.Tokens with
                | x::xs -> Some(x)
                | _ -> None
            member x.PeekExpr() =
                match x.Stack with
                | x::xs -> Some(x)
                | _ -> None
            member x.Advance() = 
                match !tokens with
                | x::xs -> current := Some(x); tokens := xs; prior := x::!prior;  true
                |_ -> current := None; false }

    /// Helper testing for an anonymous method
    let private isAnon expr = match expr with | MutableExpr.AnonymousFunc(_, _) -> true | _ -> false

    let private isIdent expr = match expr with | Some(MutableExpr.Identifier(_, _)) -> true | _ -> false

    /// Pushes a new expression onto the stack and advances to the next token
    let inline private pushExprNoAdvance (ps:IParseState) (expr:MutableExpr) = ps.PushExpr expr; true

    /// Pushes a new expression onto the stack and advances to the next token
    let inline private pushExpr (ps:IParseState) (expr:MutableExpr) = ps.PushExpr expr; ps.Advance() |> ignore; true

    /// Adds a failed token to the stack
    let private fail (ps:IParseState) (tok:Token) (code:int) (msg:string) =
        ps.AddError(ParseError(code, msg, tok))
        ps.PushExpr(Invalid(tok)); ps.Advance() |> ignore
        false

    /// Produces an And expression from the given expression list
    let private parseAnd ps exprs token = MutableExpr.And(exprs, token)
    
    /// Produces an Or expression from the given expression list
    let private parseOr ps exprs token = MutableExpr.Or(exprs, token)

    /// Adds a parse error to the parse state, and returns an invalid expr
    let private failAndReturn (ps:IParseState) code msg token =
        ps.AddError(ParseError(code, msg, token))
        Invalid(token)

    /// Produces the expression list as an if(expr, then, else) block.
    let private parseIfThenElse ps exprs token =
        let arr = List.toArray exprs
        if arr.Length <> 3 then 
            failAndReturn ps 
                          IncorrectNumberOfArgs 
                          "Incorrect number of arguments to if, must be of the form if(test, then, else)" 
                          token
        else
            MutableExpr.IfThenElse(arr.[0], arr.[1], arr.[2], token)

    /// Produced a filter expression from the given expression list
    let private parseFilter ps exprs token =
        if (List.length exprs) <> 2 then 
            failAndReturn ps 
                          IncorrectNumberOfArgs 
                          "Incorrect number of arguments to filter, must be of the form filter(expr, {filter_func})" 
                          token
        else 
            match exprs with
            | [expr1; expr2] when isAnon expr2 -> MutableExpr.Filter(expr1, expr2, token)
            | _ -> failAndReturn ps IncorrectArgType "Incorrect arguments to filter" token
 
    /// Produces an array expression from the inputs
    let private parseArray ps (exprs: MutableExpr list) token =
        if exprs.IsEmpty then
            failAndReturn ps IncorrectNumberOfArgs "Array function must have at least one argument" token
        else
            MutableExpr.Array(exprs, token)

    /// Produced a foreach expression from the given expression list
    let private parseForEach name ps exprs token =
        let len = List.length exprs
        if len < 2 then 
            failAndReturn ps IncorrectNumberOfArgs (sprintf "Incorrect number of arguments to %s" name) token
        else
            let args = [for i in 0 .. (len - 2) -> List.item i exprs]
            let last = List.item (len - 1) exprs
            if not (isAnon last) then
                failAndReturn ps IncorrectArgType (sprintf "Incorrect arguments to %s" name) token
            else
                MutableExpr.ForEach(exprs, token)
    
    /// Validates and consumes a Not expression
    let private parseNot ps exprs token =
        if List.length exprs <> 1 then
            failAndReturn ps IncorrectNumberOfArgs "Incorrect number of arguments to not, expected not(expr)" token
        else
            MutableExpr.Not(exprs.Head, token)

    /// A map of handlers for named functions that have special consideration inside of the
    /// MutableExpr definition - note that map and foreach and equivalent (curried versions of the same handler)
    let private funcHandlers = Map.ofList(["and",     parseAnd
                                           "array",   parseArray
                                           "or",      parseOr
                                           "if",      parseIfThenElse
                                           "not",     parseNot
                                           "foreach", (parseForEach "foreach")
                                           "map",     (parseForEach "map")
                                           "filter",  parseFilter])

    /// Replaces the right hand side of a binary expression with the new expression
    let private replaceBinRight bin newExpr =
        match bin with
        | BinaryOperator(_, _, right, _) -> right := newExpr
        | _ -> failwith "Not a binary operator"

    // Parses a variable.  The expected current token at point of entry is a '[' char, So this
    // method will advance over, and expect an identifier followed by a closing ']', reporting
    // any errors if this condition is not met.
    let private parseVariable (ps:IParseState) =
        let next = ps.PeekToken()
        if next.IsNone then 
            fail ps ps.Current.Value EndOfStream "Unable to parse variable, end of stream found"
        else
            let tokenVal = next.Value
            match tokenVal with            
            | Token.Identifier(ident, _) ->
                ps.Advance() |> ignore
                let maybeRParen = ps.PeekToken()
                if maybeRParen.IsSome && (Tokens.isRSquare maybeRParen.Value) then
                    ps.Advance() |> ignore
                    Variable(ident, tokenVal) |> pushExpr ps
                else fail ps tokenVal InvalidVariableDefinition "Invalid variable definition, expected [ ident ]"
            
            | _ -> fail ps tokenVal InvalidVariableDefinition "Invalid variable definition, expected [ ident ]"

    /// Returns true if the given operator may be a unary operator
    let private maybeUnary op = 
        match op with
        | Op.Minus -> true
        | _ -> false

    /// The main method for parsing an expression and adding the result to the head of the stack.
    /// Each parse will also advance over the final token matched
    let rec private parseExpr (ps:IParseState) =
        if ps.Current.IsNone then false else
        let expr = 
            let tok = ps.Current.Value
            match tok with
            | Token.Let(_)               -> parseLetBinding ps |> ignore; parseInvocation ps
            | Token.SemiColon(_)         -> ps.Advance() |> ignore; true
            | Token.LSquare(_)           -> parseVariable ps |> ignore; parseInvocation ps
            | Token.Bool(v, _)           -> Bool(v, tok) |> pushExpr ps |> ignore; parseInvocation ps
            | Token.Identifier(ident, _) -> Identifier(ident, tok) |> pushExpr ps |> ignore; parseInvocation ps
            | Token.Number(num, _)       -> Num(num, tok)     |> pushExpr ps |> ignore; parseInvocation ps
            | Token.String(str, qt, _)   -> Str(str, qt, tok) |> pushExpr ps |> ignore; parseInvocation ps
            | Token.Operator(op, _) 
                when maybeUnary op       -> parseUnaryOrBinary op ps |> ignore; parseInvocation ps
            | Token.Operator(op, _)      -> parseBinary op ps |> ignore; parseInvocation ps
            | Token.LCurly(_)            -> parseCurly ps |> ignore; parseInvocation ps
            | Token.EOF(_)               -> ps.Advance() |> ignore; false
            | Token.LParen(_)            -> ps.Advance() |> ignore; parseGroup ps |> ignore; parseInvocation ps
            | Token.Comment(_, _)        -> ps.Advance() |> ignore; true
            | _ -> fail ps tok 1 "Unknown token type found during expression parsing"
        true

    /// Parses a grouped expression (MutableExpr.Parens)
    and private parseGroup (ps:IParseState) =
        let startToken = ps.Current.Value
        let mutable id = ps.Current
        while id.IsSome && not (Tokens.isTerminator id.Value) do
            parseExpr ps |> ignore
            id <- ps.Current
        if ps.Advance() && id.IsNone || not (Tokens.isRParen id.Value) then
            fail ps startToken UnmatchedRoundBracket "Unmatched round bracket"
        else
            let interior = ps.PopExpr()
            ps.PushExpr (MutableExpr.Parens(ref interior.Value, startToken))
            true            

    /// Parses the expressions starting with the '{' character.  The result of which
    /// can be interpreted as either a macro of the form {identifier}, or an anonymous function.
    /// Extra consideration is made for identifiers that can be treated as indexers in anonymous 
    /// functions e.g. item or item0, item9 etc.  So {item0} would become an anonymous function 
    /// instead of a macro expression
    and private parseCurly (ps:IParseState) =
        let currentToken = ps.Current.Value
        let exprs = parseExpressionList ps Tokens.isRCurly
        match exprs with
        | [MutableExpr.Identifier(ident, t)] ->
            match ident with 
            | RegExMatch "item([0-9]*)" r -> MutableExpr.AnonymousFunc(exprs, t) |> pushExprNoAdvance ps
            | _ -> MutableExpr.Macro((exprs.Head), t) |> pushExprNoAdvance ps
        | [] -> fail ps currentToken 2 "Anonymous functions must contain at least one expression"
        | _ -> MutableExpr.AnonymousFunc(exprs, currentToken) |> pushExprNoAdvance ps

    /// Parses a list of expressions at the starting token, terminating when closingTok 
    /// finds a matching end token    
    and private parseExpressionList (ps:IParseState) (closingTok:Token -> bool) =
        let mutable (acc:MutableExpr list) = []
        
        // Move past starting token
        ps.Advance() |> ignore
         
        // Consume each expression until the closing token is found (or Eof)
        while (ps.Current.IsSome) && (not (closingTok (ps.Current.Value))) do
            
            // Consume until the next comma or closing token
            while ps.Current.IsSome && not (closingTok ps.Current.Value) && 
                  not (Tokens.isComma ps.Current.Value) do
                parseExpr ps |> ignore            
        
            // Move past the comma is found
            if ps.Current.IsSome && (Tokens.isComma ps.Current.Value) then
                ps.Advance() |> ignore
            
            if not ps.Stack.IsEmpty then
                // Add head to list
                let head = ps.PopExpr()
                acc <- acc @ [head.Value]

        // Move past terminating token
        ps.Advance() |> ignore
        acc

    /// Parses an expression group. If the head of the stack is an identifier, the group is parsed as a function.
    and private parseInvocation (ps:IParseState) =

        while ps.Current.IsSome && (Tokens.isLParen ps.Current.Value) do

            // Peek the head of the stack to see what is on the left of the open bracket.
            // If an identifier is found, the result will be some kind of invocation
            let head = ps.PeekExpr()

            match head with
            | Some(MutableExpr.Identifier(ident, t)) ->
            
                ps.PopExpr() |> ignore
                let ls = parseExpressionList ps (Tokens.isRParen)

                // Use a function handler if available
                match Map.tryFind (ident.ToLower()) funcHandlers with
                | Some(func) -> func ps ls t |> ps.PushExpr |> ignore
                | None -> MutableExpr.Function(MutableExpr.Identifier(ident, t), ls, t) |> ps.PushExpr |> ignore
            | _ -> ()

        true

    /// Attempts to parse a unary expression, or a binary depending on what is on the head of the stack
    and private parseUnaryOrBinary (op:Op) (ps:IParseState) =
        let tok = ps.Current.Value
        if not (ps.Stack.IsEmpty) then
            let maybeBin = parseBinary op ps
            if maybeBin.IsSome then
                ps.PushExpr(MutableExpr.UnaryOperator(op, maybeBin.Value, tok)) |> ignore
        else
            parseUnary op ps |> ignore

    /// Parses a unary expression
    and private parseUnary (op:Op) (ps:IParseState) =
        
        let currentToken = ps.Current.Value

        if not (ps.Advance()) then 
            fail ps currentToken EndOfStream "Expected an expression to the right of operator"
        elif not (parseExpr ps) then
            fail ps ps.Current.Value EndOfStream "Unable to parse binary expression, no right hand side"
        else
            while ps.Current.IsSome && 
                (Precedence.ofToken (ps.Current.Value)) > (Precedence.ofOp op) do
                parseExpr ps |> ignore
            let expr = ps.PopExpr()
            ps.PushExpr (MutableExpr.UnaryOperator(op, expr.Value, currentToken))
            true
            
    and private isValidAssignment expr =
        match expr with
        | Some(BinaryOperator(left, op, right, _)) ->
            match op, !left with
            | Op.Eq, Identifier(_, _) -> true, Some(!left), Some(!right)
            | _ -> false, None, None
        | _ -> false, None, None

    and private parseLetBinding (ps: IParseState) =
        ps.Advance() |> ignore
        if not (parseExpr ps) then ()
        let head = ps.PeekExpr()
        if not (isIdent head) then 
            ps.AddError(ParseError(InvalidLetBinding, "Identifier expected after let binding", (head.Value.Token)))
        else
            if not (parseExpr ps) then ()
            let isValid, ident, expr = isValidAssignment (ps.PeekExpr())
            if not isValid then
                ps.AddError(ParseError(InvalidLetBinding, "Invalid expression found in let binding", (head.Value.Token)))
            else
                ps.PopExpr() |> ignore
                pushExprNoAdvance ps (Assign((ident.Value), (expr.Value), (head.Value.Token))) |> ignore

    /// Parses a binary expression starting with the given operator.
    and private parseBinary (op: Op) (ps:IParseState): MutableExpr option =
        let priorTokenLen = List.length ps.PreviousTokens
        if ps.Stack.IsEmpty || 
           (priorTokenLen >= 2 && (Tokens.isComma (List.item 1 ps.PreviousTokens) 
           || Precedence.ofToken (List.item 1 ps.PreviousTokens) > 0)) then

            ps.Advance() |> ignore
            parseExpr ps |> ignore

            while ps.Current.IsSome && (Precedence.ofToken (ps.Current.Value)) > (Precedence.ofOp op) do
                parseExpr ps |> ignore

            ps.PopExpr()
        else
            let lhs = ps.PopExpr()
            if lhs.IsNone then
                fail ps ps.Current.Value 2 "Left hand side of expression is empty" |> ignore                
            else
                if not (ps.Advance()) then 
                    fail ps 
                         (lhs.Value.Token) 
                         EndOfStream 
                         "Unable to parse binary expression, no right hand side"  
                    |> ignore                    
                else
                    let binary = BinaryOperator(ref lhs.Value, 
                                                op, 
                                                ref (Invalid(ps.Current.Value)), 
                                                ps.Current.Value)
                    ps.PushExpr binary

                    if not (parseExpr ps) then 
                        fail ps 
                             ps.Current.Value 
                             EndOfStream 
                             "Unable to parse binary expression, no right hand side" 
                        |> ignore
                    else
                        while ps.Current.IsSome && (Precedence.ofToken (ps.Current.Value)) > (Precedence.ofOp op) do
                            parseExpr ps |> ignore

                        replaceBinRight binary (ps.PopExpr()).Value
            None
    
    /// Performs a parse on a list of tokens            
    [<CompiledName("ParseTokens")>]
    let private parseTokens (tokens: Token list) =
        let rec parserec (ps:IParseState) =
            if (parseExpr ps) then parserec ps else ps

        let ps = createParseState tokens
        try
            if ps.Advance() then parserec ps else ps
        with e -> 
            // Add a ParseError to the parse state to represent the internal exception
            ps.AddError(ParseError(InternalError, e.Message, tokens.Head))
            ps

    /// This is the main parsing method responsible for converting a string into 
    /// an expression tree.
    [<CompiledName("ParseString")>]
    let parseString (str:string) (env: IEnvironment) =
        let tokens = (Scanner.stringScanner(str)) |> Lexer.lex
        let tokenErrors = Validator.validateTokens tokens
        if not tokenErrors.IsEmpty then 
            ParseResult(List.empty, tokenErrors) 
        else
            let ps = tokens |> parseTokens
            let expressions = List.rev ps.Stack |> toPublicAst
            let exprErrors = Validator.validateExpressions expressions env
            Seq.iter (fun err -> ps.AddError(err)) exprErrors
            ParseResult(expressions, ps.Errors)