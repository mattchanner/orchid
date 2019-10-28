namespace Orchid

open Orchid.Expressions

open Xunit
open FsUnit.Xunit

module ScannerTests =

    let lex str = 
        Scanner.stringScanner str 
        |> Lexer.lex

    let errm (tokens: Token list) =
        tokens 
        |> List.map (fun x -> x.ToString() + " ")
        |> List.reduce (+)
        |> sprintf "Failed to match expected token list: %s"

    [<Fact>]
    let ``Can read let binding``() =
        lex "let x = 2;" |> function
        | [ 
            Token.Let(_)
            Token.Identifier("x", _)
            Token.Operator(Op.Eq, _)
            Token.Number(2.0, _)
            Token.SemiColon(_)
          ] -> Assert.True(true)
        | _ as tokens -> failwith (errm tokens)

    [<Fact>]
    let ``Can read multiple statements`` () =
        lex "let x = 2; let y = 5;" |> function
        | [
            Token.Let(_)
            Token.Identifier("x", _)
            Token.Operator(Op.Eq, _)
            Token.Number(2.0, _)
            Token.SemiColon(_)
            Token.Let(_)
            Token.Identifier("y", _)
            Token.Operator(Op.Eq, _)
            Token.Number(5.0, _)
            Token.SemiColon(_)
          ] -> Assert.True(true)
        | _ as tokens -> failwith (errm tokens)



