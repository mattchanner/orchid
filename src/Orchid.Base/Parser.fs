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
/// require AST rewriting via ref cells which should only be supported during parsing.
/// Note: BinaryOperator uses Ref for left/right to support operator precedence rewriting.
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

/// Immutable parse state record - replaces mutable IParseState interface
type private ParseState = {
    Current: Token option
    Tokens: Token list
    Stack: MutableExpr list
    PreviousTokens: Token list
    Errors: ParseError list
}

/// Operations on ParseState - all return new state without mutation
module private ParseState =

    /// Create initial parse state from token list
    let create (tokens: Token list) : ParseState =
        { Current = None
          Tokens = tokens
          Stack = []
          PreviousTokens = []
          Errors = [] }

    /// Advance to the next token, returns (newState, didAdvance)
    let advance (state: ParseState) : ParseState * bool =
        match state.Tokens with
        | token :: rest ->
            { state with
                Current = Some token
                Tokens = rest
                PreviousTokens = token :: state.PreviousTokens }, true
        | [] ->
            { state with Current = None }, false

    /// Push an expression onto the stack
    let pushExpr (expr: MutableExpr) (state: ParseState) : ParseState =
        { state with Stack = expr :: state.Stack }

    /// Pop an expression from the stack, returns (newState, poppedExpr)
    let popExpr (state: ParseState) : ParseState * MutableExpr option =
        match state.Stack with
        | expr :: rest -> { state with Stack = rest }, Some expr
        | [] -> state, None

    /// Peek at the top expression without modifying state
    let peekExpr (state: ParseState) : MutableExpr option =
        List.tryHead state.Stack

    /// Peek at the next token without modifying state
    let peekToken (state: ParseState) : Token option =
        List.tryHead state.Tokens

    /// Add an error to the state
    let addError (error: ParseError) (state: ParseState) : ParseState =
        { state with Errors = error :: state.Errors }

    /// Add an error with code, message, and token
    let addErrorWith (code: int) (msg: string) (token: Token) (state: ParseState) : ParseState =
        addError (ParseError(code, msg, token)) state

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

    /// Helper testing for an anonymous method
    let private isAnon expr = match expr with | MutableExpr.AnonymousFunc(_, _) -> true | _ -> false

    let private isIdent expr = match expr with | Some(MutableExpr.Identifier(_, _)) -> true | _ -> false

    /// Pushes expression onto stack and advances to next token
    let private pushExprAndAdvance (expr: MutableExpr) (state: ParseState) : ParseState * bool =
        let state = ParseState.pushExpr expr state
        ParseState.advance state

    /// Pushes expression onto stack without advancing
    let private pushExprNoAdvance (expr: MutableExpr) (state: ParseState) : ParseState * bool =
        ParseState.pushExpr expr state, true

    /// Records an error and pushes Invalid expression
    let private fail (tok: Token) (code: int) (msg: string) (state: ParseState) : ParseState * bool =
        let state = ParseState.addError (ParseError(code, msg, tok)) state
        let state = ParseState.pushExpr (Invalid tok) state
        let state, _ = ParseState.advance state
        state, false

    /// Produces an And expression from the given expression list
    let private parseAnd state exprs token = MutableExpr.And(exprs, token)

    /// Produces an Or expression from the given expression list
    let private parseOr state exprs token = MutableExpr.Or(exprs, token)

    /// Adds a parse error to the parse state, and returns an invalid expr
    let private failAndReturn (state: ParseState) code msg token =
        ParseState.addError (ParseError(code, msg, token)) state, Invalid(token)

    /// Produces the expression list as an if(expr, then, else) block.
    let private parseIfThenElse state exprs token =
        let arr = List.toArray exprs
        if arr.Length <> 3 then
            failAndReturn state
                          IncorrectNumberOfArgs
                          "Incorrect number of arguments to if, must be of the form if(test, then, else)"
                          token
        else
            state, MutableExpr.IfThenElse(arr.[0], arr.[1], arr.[2], token)

    /// Produced a filter expression from the given expression list
    let private parseFilter state exprs token =
        if (List.length exprs) <> 2 then
            failAndReturn state
                          IncorrectNumberOfArgs
                          "Incorrect number of arguments to filter, must be of the form filter(expr, {filter_func})"
                          token
        else
            match exprs with
            | [expr1; expr2] when isAnon expr2 -> state, MutableExpr.Filter(expr1, expr2, token)
            | _ -> failAndReturn state IncorrectArgType "Incorrect arguments to filter" token

    /// Produces an array expression from the inputs
    let private parseArray state (exprs: MutableExpr list) token =
        if exprs.IsEmpty then
            failAndReturn state IncorrectNumberOfArgs "Array function must have at least one argument" token
        else
            state, MutableExpr.Array(exprs, token)

    /// Produced a foreach expression from the given expression list
    let private parseForEach name state exprs token =
        let len = List.length exprs
        if len < 2 then
            failAndReturn state IncorrectNumberOfArgs (sprintf "Incorrect number of arguments to %s" name) token
        else
            let last = List.last exprs
            if not (isAnon last) then
                failAndReturn state IncorrectArgType (sprintf "Incorrect arguments to %s" name) token
            else
                state, MutableExpr.ForEach(exprs, token)

    /// Validates and consumes a Not expression
    let private parseNot state exprs token =
        if List.length exprs <> 1 then
            failAndReturn state IncorrectNumberOfArgs "Incorrect number of arguments to not, expected not(expr)" token
        else
            state, MutableExpr.Not(exprs.Head, token)

    /// A map of handlers for named functions that have special consideration inside of the
    /// MutableExpr definition - note that map and foreach and equivalent (curried versions of the same handler)
    let private funcHandlers =
        Map.ofList([
            "and",     fun state exprs token -> state, parseAnd state exprs token
            "array",   parseArray
            "or",      fun state exprs token -> state, parseOr state exprs token
            "if",      parseIfThenElse
            "not",     parseNot
            "foreach", parseForEach "foreach"
            "map",     parseForEach "map"
            "filter",  parseFilter
        ])

    /// Replaces the right hand side of a binary expression with the new expression
    /// Note: This is one place where we still use mutation (Ref) for operator precedence handling
    let private replaceBinRight bin newExpr =
        match bin with
        | BinaryOperator(_, _, right, _) -> right := newExpr; true
        | _ -> false

    /// Returns true if the given operator may be a unary operator
    let private maybeUnary op =
        match op with
        | Op.Minus -> true
        | _ -> false

    // Parses a variable.  The expected current token at point of entry is a '[' char
    let private parseVariable (state: ParseState) : ParseState * bool =
        let next = ParseState.peekToken state
        match next with
        | None ->
            fail state.Current.Value EndOfStream "Unable to parse variable, end of stream found" state
        | Some tokenVal ->
            match tokenVal with
            | Token.Identifier(ident, _) ->
                let state, _ = ParseState.advance state
                let maybeRSquare = ParseState.peekToken state
                match maybeRSquare with
                | Some tok when Tokens.isRSquare tok ->
                    let state, _ = ParseState.advance state
                    pushExprAndAdvance (Variable(ident, tokenVal)) state
                | _ ->
                    fail tokenVal InvalidVariableDefinition "Invalid variable definition, expected [ ident ]" state
            | _ ->
                fail tokenVal InvalidVariableDefinition "Invalid variable definition, expected [ ident ]" state

    /// The main method for parsing an expression and adding the result to the head of the stack.
    /// Each parse will also advance over the final token matched
    let rec private parseExpr (state: ParseState) : ParseState * bool =
        match state.Current with
        | None -> state, false
        | Some tok ->
            match tok with
            | Token.Let(_) ->
                let state = parseLetBinding state
                parseInvocation state
            | Token.SemiColon(_) ->
                let state, _ = ParseState.advance state
                state, true
            | Token.LSquare(_) ->
                let state, _ = parseVariable state
                parseInvocation state
            | Token.Bool(v, _) ->
                let state, _ = pushExprAndAdvance (Bool(v, tok)) state
                parseInvocation state
            | Token.Identifier(ident, _) ->
                let state, _ = pushExprAndAdvance (Identifier(ident, tok)) state
                parseInvocation state
            | Token.Number(num, _) ->
                let state, _ = pushExprAndAdvance (Num(num, tok)) state
                parseInvocation state
            | Token.String(str, qt, _) ->
                let state, _ = pushExprAndAdvance (Str(str, qt, tok)) state
                parseInvocation state
            | Token.Operator(op, _) when maybeUnary op ->
                let state = parseUnaryOrBinary op state
                parseInvocation state
            | Token.Operator(op, _) ->
                let state, _ = parseBinary op state
                parseInvocation state
            | Token.LCurly(_) ->
                let state, _ = parseCurly state
                parseInvocation state
            | Token.EOF(_) ->
                let state, _ = ParseState.advance state
                state, false
            | Token.LParen(_) ->
                let state, _ = ParseState.advance state
                let state, _ = parseGroup state
                parseInvocation state
            | Token.Comment(_, _) ->
                let state, _ = ParseState.advance state
                state, true
            | _ ->
                fail tok 1 "Unknown token type found during expression parsing" state

    /// Parses a grouped expression (MutableExpr.Parens)
    and private parseGroup (state: ParseState) : ParseState * bool =
        let startToken = state.Current.Value

        let rec loop state =
            match state.Current with
            | Some tok when not (Tokens.isTerminator tok) ->
                let state, _ = parseExpr state
                loop state
            | _ -> state

        let state = loop state
        let finalToken = state.Current
        let state, _ = ParseState.advance state

        match finalToken with
        | Some tok when Tokens.isRParen tok ->
            let state, interior = ParseState.popExpr state
            match interior with
            | Some expr ->
                let state = ParseState.pushExpr (MutableExpr.Parens(ref expr, startToken)) state
                state, true
            | None ->
                fail startToken UnmatchedRoundBracket "Unmatched round bracket" state
        | _ ->
            fail startToken UnmatchedRoundBracket "Unmatched round bracket" state

    /// Parses the expressions starting with the '{' character.
    and private parseCurly (state: ParseState) : ParseState * bool =
        let currentToken = state.Current.Value
        let state, exprs = parseExpressionList Tokens.isRCurly state
        match exprs with
        | [MutableExpr.Identifier(ident, t)] ->
            match ident with
            | RegExMatch "item([0-9]*)" _ ->
                pushExprNoAdvance (MutableExpr.AnonymousFunc(exprs, t)) state
            | _ ->
                pushExprNoAdvance (MutableExpr.Macro(exprs.Head, t)) state
        | [] ->
            fail currentToken 2 "Anonymous functions must contain at least one expression" state
        | _ ->
            pushExprNoAdvance (MutableExpr.AnonymousFunc(exprs, currentToken)) state

    /// Parses a list of expressions at the starting token, terminating when closingTok finds a matching end token
    and private parseExpressionList (closingTok: Token -> bool) (state: ParseState) : ParseState * MutableExpr list =
        // Move past starting token
        let state, _ = ParseState.advance state

        let rec outerLoop state acc =
            match state.Current with
            | None -> state, List.rev acc
            | Some tok when closingTok tok -> state, List.rev acc
            | _ ->
                // Parse expressions until comma or closing token
                let rec innerLoop state =
                    match state.Current with
                    | Some tok when not (closingTok tok) && not (Tokens.isComma tok) ->
                        let state, _ = parseExpr state
                        innerLoop state
                    | _ -> state

                let state = innerLoop state

                // Skip comma if present
                let state =
                    match state.Current with
                    | Some tok when Tokens.isComma tok ->
                        let state, _ = ParseState.advance state
                        state
                    | _ -> state

                // Pop expression and add to accumulator
                match state.Stack with
                | [] -> outerLoop state acc
                | _ ->
                    let state, expr = ParseState.popExpr state
                    match expr with
                    | Some e -> outerLoop state (e :: acc)
                    | None -> outerLoop state acc

        let state, exprs = outerLoop state []
        // Move past terminating token
        let state, _ = ParseState.advance state
        state, exprs

    /// Parses an expression group. If the head of the stack is an identifier, the group is parsed as a function.
    and private parseInvocation (state: ParseState) : ParseState * bool =
        let rec loop state =
            match state.Current with
            | Some tok when Tokens.isLParen tok ->
                let head = ParseState.peekExpr state
                match head with
                | Some(MutableExpr.Identifier(ident, t)) ->
                    let state, _ = ParseState.popExpr state
                    let state, ls = parseExpressionList Tokens.isRParen state

                    // Use a function handler if available
                    let state, expr =
                        match Map.tryFind (ident.ToLower()) funcHandlers with
                        | Some(func) -> func state ls t
                        | None -> state, MutableExpr.Function(MutableExpr.Identifier(ident, t), ls, t)

                    let state = ParseState.pushExpr expr state
                    loop state
                | _ -> state, true
            | _ -> state, true

        loop state

    /// Attempts to parse a unary expression, or a binary depending on what is on the head of the stack
    and private parseUnaryOrBinary (op: Op) (state: ParseState) : ParseState =
        let tok = state.Current.Value
        if not (state.Stack.IsEmpty) then
            let state, maybeBin = parseBinary op state
            match maybeBin with
            | Some expr ->
                ParseState.pushExpr (MutableExpr.UnaryOperator(op, expr, tok)) state
            | None -> state
        else
            let state, _ = parseUnary op state
            state

    /// Parses a unary expression
    and private parseUnary (op: Op) (state: ParseState) : ParseState * bool =
        let currentToken = state.Current.Value
        let state, advanced = ParseState.advance state

        if not advanced then
            fail currentToken EndOfStream "Expected an expression to the right of operator" state
        else
            let state, parsed = parseExpr state
            if not parsed then
                fail state.Current.Value EndOfStream "Unable to parse binary expression, no right hand side" state
            else
                let rec precedenceLoop state =
                    match state.Current with
                    | Some tok when Precedence.ofToken tok > Precedence.ofOp op ->
                        let state, _ = parseExpr state
                        precedenceLoop state
                    | _ -> state

                let state = precedenceLoop state
                let state, expr = ParseState.popExpr state
                match expr with
                | Some e ->
                    let state = ParseState.pushExpr (MutableExpr.UnaryOperator(op, e, currentToken)) state
                    state, true
                | None ->
                    fail currentToken EndOfStream "Unable to parse unary expression" state

    and private isValidAssignment expr =
        match expr with
        | Some(BinaryOperator(left, op, right, _)) ->
            match op, !left with
            | Op.Eq, Identifier(_, _) -> true, Some(!left), Some(!right)
            | _ -> false, None, None
        | _ -> false, None, None

    and private parseLetBinding (state: ParseState) : ParseState =
        let state, _ = ParseState.advance state
        let state, parsed = parseExpr state
        if not parsed then state
        else
            let head = ParseState.peekExpr state
            if not (isIdent head) then
                ParseState.addError (ParseError(InvalidLetBinding, "Identifier expected after let binding", head.Value.Token)) state
            else
                let state, _ = parseExpr state
                let isValid, ident, expr = isValidAssignment (ParseState.peekExpr state)
                if not isValid then
                    ParseState.addError (ParseError(InvalidLetBinding, "Invalid expression found in let binding", head.Value.Token)) state
                else
                    let state, _ = ParseState.popExpr state
                    let state = ParseState.pushExpr (Assign(ident.Value, expr.Value, head.Value.Token)) state
                    state

    /// Parses a binary expression starting with the given operator.
    and private parseBinary (op: Op) (state: ParseState) : ParseState * MutableExpr option =
        let priorSecondToken = List.tryItem 1 state.PreviousTokens
        let isPriorCommaOrOperator =
            match priorSecondToken with
            | Some tok -> Tokens.isComma tok || Precedence.ofToken tok > 0
            | None -> false

        if state.Stack.IsEmpty || isPriorCommaOrOperator then
            let state, _ = ParseState.advance state
            let state, _ = parseExpr state

            let rec precedenceLoop state =
                match state.Current with
                | Some tok when Precedence.ofToken tok > Precedence.ofOp op ->
                    let state, _ = parseExpr state
                    precedenceLoop state
                | _ -> state

            let state = precedenceLoop state
            let state, expr = ParseState.popExpr state
            state, expr
        else
            let state, lhs = ParseState.popExpr state
            match lhs with
            | None ->
                let state, _ = fail state.Current.Value 2 "Left hand side of expression is empty" state
                state, None
            | Some lhsExpr ->
                let state, advanced = ParseState.advance state
                if not advanced then
                    let state, _ = fail lhsExpr.Token EndOfStream "Unable to parse binary expression, no right hand side" state
                    state, None
                else
                    let binary = BinaryOperator(ref lhsExpr, op, ref (Invalid state.Current.Value), state.Current.Value)
                    let state = ParseState.pushExpr binary state

                    let state, parsed = parseExpr state
                    if not parsed then
                        let state, _ = fail state.Current.Value EndOfStream "Unable to parse binary expression, no right hand side" state
                        state, None
                    else
                        let rec precedenceLoop state =
                            match state.Current with
                            | Some tok when Precedence.ofToken tok > Precedence.ofOp op ->
                                let state, _ = parseExpr state
                                precedenceLoop state
                            | _ -> state

                        let state = precedenceLoop state
                        let state, rhs = ParseState.popExpr state
                        match rhs with
                        | Some rhsExpr -> replaceBinRight binary rhsExpr |> ignore
                        | None -> ()
                        state, None

    /// Performs a parse on a list of tokens
    [<CompiledName("ParseTokens")>]
    let private parseTokens (tokens: Token list) : ParseState =
        let rec loop state =
            let state, continue' = parseExpr state
            if continue' then loop state else state

        let initialState = ParseState.create tokens
        try
            let state, advanced = ParseState.advance initialState
            if advanced then loop state else state
        with e ->
            // Add a ParseError to the parse state to represent the internal exception
            ParseState.addError (ParseError(InternalError, e.Message, tokens.Head)) initialState

    /// This is the main parsing method responsible for converting a string into
    /// an expression tree.
    [<CompiledName("ParseString")>]
    let parseString (str:string) (env: IEnvironment) =
        let tokens = (Scanner.stringScanner(str)) |> Lexer.lex
        let tokenErrors = Validator.validateTokens tokens
        if not tokenErrors.IsEmpty then
            ParseResult(List.empty, tokenErrors)
        else
            let finalState = parseTokens tokens
            let expressions = List.rev finalState.Stack |> toPublicAst
            let exprErrors = Validator.validateExpressions expressions env
            let allErrors = List.append finalState.Errors (List.ofSeq exprErrors)
            ParseResult(expressions, allErrors)
