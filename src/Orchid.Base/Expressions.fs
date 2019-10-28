namespace Orchid.Expressions

open System.Globalization
open System.Text
open System.Runtime.CompilerServices

/// A scanner interface, allowing various sources of data to be scanned.
type IScanner =

    /// Advances to the next position, returning a value indicating whether there are more elements in the stream
    abstract member Advance: unit -> bool

    /// Returns a value indicating whether the source stream contains any more characters.
    abstract member HasNext: unit -> bool

    /// Returns the current character from the stream.
    abstract member Current: char option

    /// Peeks at the next character in the stream, returning an option, where None indicates the scanner
    /// is at then end of the stream.
    abstract member Peek: unit -> char option

    /// Rewinds back one step
    abstract member Rewind: unit -> unit

/// Represents the location of the token within the source string.
type TokenLocation = { Line: int; Column: int; Range: int }

/// This enumeration represents the known operators
type Op =
    | Plus  = 0
    | Minus = 1
    | Div   = 2
    | Mul   = 3
    | Pow   = 4
    | Mod   = 5
    | Gt    = 6
    | Gte   = 7
    | Lt    = 8
    | Lte   = 9
    | Eq    = 10
    | NEq   = 11 
    | Or    = 12
    | And   = 13
    
/// The quote character used in a string literal
type QuoteType = Single = 0 | Double = 1

/// Represents the various tokens that the lexer can produce
type Token =
    | Let         of TokenLocation
    | Number      of double * TokenLocation
    | Bool        of bool   * TokenLocation
    | String      of string * QuoteType * TokenLocation
    | Comment     of string * TokenLocation
    | Identifier  of string * TokenLocation
    | Operator    of Op     * TokenLocation
    | Comma       of TokenLocation
    | LParen      of TokenLocation
    | RParen      of TokenLocation
    | LCurly      of TokenLocation
    | RCurly      of TokenLocation
    | LSquare     of TokenLocation
    | RSquare     of TokenLocation
    | SemiColon   of TokenLocation
    | EOF         of TokenLocation
    member x.Location =
        match x with
        | Let(tl)           -> tl
        | Number(_, tl)     -> tl
        | Bool(_, tl)       -> tl
        | String(_, _, tl)  -> tl
        | Comment(_, tl)    -> tl
        | Identifier(_, tl) -> tl
        | Operator(_, tl)   -> tl
        | Comma(tl)         -> tl
        | LParen(tl)        -> tl
        | RParen(tl)        -> tl
        | LCurly(tl)        -> tl
        | RCurly(tl)        -> tl
        | LSquare(tl)       -> tl
        | RSquare(tl)       -> tl
        | SemiColon(tl)     -> tl
        | EOF(tl)           -> tl
    override x.ToString() =
        match x with
        | Let(tl)           -> "Let"
        | Number(n, tl)     -> sprintf "Num(%f)" n
        | Bool(v, tl)       -> sprintf "Num(%b)" v
        | String(s, _, tl)  -> sprintf "Str(%s)" s
        | Comment(c, tl)    -> sprintf "Comment(%s)" c
        | Identifier(n, tl) -> sprintf "Ident(%s)" n
        | Operator(op, tl)   -> sprintf "Op(%A)" op
        | Comma(tl)         -> "Comma"
        | LParen(tl)        -> "LParen"
        | RParen(tl)        -> "RParen"
        | LCurly(tl)        -> "LCurly"
        | RCurly(tl)        -> "RCurly"
        | LSquare(tl)       -> "LSquare"
        | RSquare(tl)       -> "RSquare"
        | SemiColon(tl)     -> "SemiColon"
        | EOF(tl)           -> "<<EOF>>"

/// The AST definition of the expression language
type Expr =
    | Assign         of Expr * Expr * Token           // Assign the result of an expression to a local variable (let binding)
    | Parens         of Expr * Token                  // Expression held in parenthesis       
    | Str            of string * QuoteType * Token    // String value with quotation type retained
    | Bool           of bool * Token                  // Boolean expression
    | Num            of double * Token                // Numeric expression
    | Identifier     of string * Token                // Identifier
    | Variable       of string * Token                // A reference to a variable
    | BinaryOperator of Expr * Op * Expr * Token      // Binary operator
    | UnaryOperator  of Op * Expr * Token             // Unary operator
    | AnonymousFunc  of Expr list * Token             // A list of expressions held within { }
    | Macro          of Expr * Token                  // A macro expression held within { }
    | Function       of Expr * Expr list * Token      // A call to a function
    | IfThenElse     of Expr * Expr * Expr * Token    // If statement
    | Filter         of Expr * Expr * Token           // Applies a filter expression to an input array
    | ForEach        of Expr list * Token             // Maps a function expression to elements of an n dimension array
    | Array          of Expr list * Token             // Produces an array from a list of expressions
    | Not            of Expr * Token                  // Logical not
    | Or             of Expr list * Token             // Logical or
    | And            of Expr list * Token             // Logical and
    | Invalid        of Token                         // Invalid token marker
    member x.Token =
        match x with
        | Assign(_, _, t)            -> t
        | Array(_, t)                -> t
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

/// Represents a parsing error
[<Class>]
type ParseError(code:int, msg:string, token:Token) =
    
    /// The error code for this parser error
    member x.Code with get() = code

    /// The error message
    member x.Message with get() = msg

    /// The associated token
    member x.Token with get() = token

/// Represents the result of the parse process
[<Class>]
type ParseResult(expressions:Expr list, errors: ParseError list) =
     
    /// The top level expressions
    member x.Expressions with get() = expressions

    /// The errors
    member x.Errors with get() = errors

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<Extension>] 
module Expr =

    /// Returns a string representation of the given operator
    [<CompiledName("OpString")>]
    let opString (op:Op) =
        match op with
        | Op.Div    -> " / "
        | Op.Eq     -> " = "
        | Op.Gt     -> " > "
        | Op.Gte    -> " >= "
        | Op.Lt     -> " < "
        | Op.Lte    -> " <= "
        | Op.Minus  -> " - "
        | Op.Mod    -> " % "
        | Op.Mul    -> " * "
        | Op.NEq    -> " <> "
        | Op.Plus   -> " + "
        | Op.Pow    -> " ^ "
        | Op.Or     -> " || "
        | Op.And    -> " && "
        | _ -> failwith "Undefined enum value"

    /// Returns a string representation of the given expression, the result of which is a string builder.
    [<CompiledName("ExprString")>]
    let rec expString (expr:Expr) (sb:StringBuilder) =
        let rec printexprec (expr:Expr) (sb: StringBuilder) =
            match expr with
            | Expr.Assign(name, expr, _) -> sb.AppendFormat("let {0} := ", name) |> printexplist [expr] |> ignore; sb.Append(";"); 
            | Expr.And(ls,_)            -> printfunc "and" ls sb
            | Expr.AnonymousFunc(e, _)  -> sb.Append("{") |> (printexplist e) |> ignore; sb.Append("}")
            | Expr.Array(e, _)          -> printfunc "array" e sb
            | Expr.Macro(e, _)          -> sb.Append("{") |> (printexprec e) |> ignore; sb.Append("}")
            | Expr.BinaryOperator(lhs, op, rhs, _)  -> 
                (printexprec lhs sb) 
                    |> (fun (sb:StringBuilder) -> sb.Append((opString op))) 
                    |> (printexprec rhs)

            | Expr.Bool(b,_)            -> sb.Append(" ").Append(b.ToString()).Append(" ")
            | Expr.Filter(exp1, exp2,_) -> printfunc "filter" [exp1; exp2] sb
            | Expr.ForEach(ls, _)       -> printfunc "foreach" ls sb
            | Expr.Function(Expr.Identifier(name, _), ls,_) 
                                        -> printfunc name ls sb
            | Expr.Identifier(name, _)  -> sb.Append(name)
            | Expr.IfThenElse(arg1, arg2, arg3, _) 
                                        -> printfunc "if" [arg1; arg2; arg3] sb
            | Expr.Not(arg, _)          -> printfunc "not" [arg] sb
            | Expr.Num(num, _)          -> sb.Append(num.ToString(CultureInfo.InvariantCulture))
            | Expr.Or(args, _)          -> printfunc "or" args sb
            | Expr.Str(str, quote, _)   -> 
                let q = (if quote = QuoteType.Double then "\"" else "'")
                sb.Append(q).Append(str).Append(q)
            | Expr.UnaryOperator(op, expr, _) 
                -> sb.Append((opString op)) |> (printexprec expr)
            | Expr.Variable(var, _)     -> sb.Append("[").Append(var).Append("]")
            | _ -> sb
        let sb = (printexprec expr sb) 
        sb

    /// Prints a function to the string builder
    and private printfunc (name:string) (ls:Expr list) (sb:StringBuilder) =
        sb.Append(name) |> ignore
        if (List.length ls) > 0 then
            sb.Append("(") |> (printexplist ls) |> ignore
            sb.Append(")")
        else sb

    /// Prints an expression list to the string builder
    and private printexplist (ls:Expr list) (sb:StringBuilder) =
        let size = List.length ls
        let i = ref 0
        for expr in ls do
            (expString expr sb) |> ignore
            if !i < size - 1 then sb.Append(",") |> ignore
            incr i
        sb

    /// Converts the given expression back into a string representation of itself
    [<Extension>] 
    let ToExpressionString(x: Expr) = (expString x (StringBuilder())).ToString()

