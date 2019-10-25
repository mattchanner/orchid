namespace Orchid

open System.Reflection

open Orchid.Expressions
open Orchid.Runtime

open Xunit
open FsUnit.Xunit

module ParserTests =
    
    let environment = 
        EnvironmentSetup.SetupFromPaths(
            Assembly.GetExecutingAssembly().Location, 
            Assembly.GetExecutingAssembly().Location)

    let parse str =
        let result = Parser.parseString str environment
        result.Expressions

    [<Fact>]
    let ``Can parse floating point number`` () =
        parse "1.234" |> function
        | [ Expr.Num(value, _) ] -> value |> should equal 1.234
        | _ -> failwith "Expected a single numeric expression"

    [<Fact>]
    let ``Can parse true``() =
        parse "true" |> function
        | [ Expr.Bool(value, _) ] -> value |> should be True
        | _ -> failwith "Unexpected expressions list"

    [<Fact>]
    let ``Can parse false``() =
        parse "false" |> function
        | [ Expr.Bool(value, _) ] -> value |> should be False
        | _ -> failwith "Unexpected expressions list"

    [<Fact>]
    let ``Can parse single quoted string`` () =
        parse "'A single quoted string'" |> function
        | [ Expr.Str(str, quoteType, _) ] -> 
            str |> should equal "A single quoted string"
            quoteType |> should equal QuoteType.Single
        | _ -> failwith "Unexpected expressions list"

    [<Fact>]
    let ``Can parse double quoted string`` () =
        parse "\"A double quoted string\"" |> function
        | [ Expr.Str(str, quoteType, _) ] -> 
            str |> should equal "A double quoted string"
            quoteType |> should equal QuoteType.Double
        | _ -> failwith "Unexpected expressions list"

    [<Fact>]
    let ``Can parse unary number``() =
        parse "-123.456" |> function
        | [ Expr.UnaryOperator(op, Expr.Num(num, _), _) ] -> 
            op |> should equal Op.Minus
            num |> should equal 123.456
        | _ -> failwith "Unexpected expressions list"

    [<Fact>]
    let ``Can parse simple binary plus``() =
        parse "34 + 856" |> function
        | [ Expr.BinaryOperator(Expr.Num(lhs, _), op, Expr.Num(rhs, _), _) ] ->
            lhs |> should equal 34.0
            op |> should equal Op.Plus
            rhs |> should equal 856.0
        | _ -> failwith "Unexpected expression list"

    [<Fact>]
    let ``Can parse multiple binary expressions with equal precedence``() =
        parse "1 + 2 - 3" |> function
        | [ Expr.BinaryOperator(
                Expr.BinaryOperator(
                    Expr.Num(1.0, _),
                    Op.Plus,
                    Expr.Num(2.0, _),
                    _),
                Op.Minus,
                Expr.Num(3.0, _),
                _)] -> Assert.True(true)
        | _ as ls -> failwith ("Unexpected expression list: " + (ls.ToString()))

    [<Fact>]
    let ``Can parse function``() =
        parse "average(1, 2, 3)" |> function
        | [
            Expr.Function(
                Expr.Identifier(name, _),
                [
                    Expr.Num(1.0, _) 
                    Expr.Num(2.0, _)
                    Expr.Num(3.0, _)
                ],
                _)
            ]-> name |> should equal "average"
        | _ as ls -> failwith ("Unexpected expression list: " + (ls.ToString()))