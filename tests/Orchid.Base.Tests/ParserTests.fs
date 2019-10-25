namespace Orchid

open System.Reflection

open Orchid.Expressions
open Orchid.Runtime

open Xunit

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
        | [ Expr.Num(value, _) ] -> Assert.Equal(value, 1.234)
        | _ -> failwith "Expected a single numeric expression"

    [<Fact>]
    let ``Can parse true``() =
        parse "true" |> function
        | [ Expr.Bool(value, _) ] -> Assert.True(value)
        | _ -> failwith "Unexpected expressions list"

    [<Fact>]
    let ``Can parse false``() =
        parse "false" |> function
        | [ Expr.Bool(value, _) ] -> Assert.False(value)
        | _ -> failwith "Unexpected expressions list"

    [<Fact>]
    let ``Can parse single quoted string`` () =
        parse "'A single quoted string'" |> function
        | [ Expr.Str(str, quoteType, _) ] -> 
            Assert.Equal("A single quoted string", str)
            Assert.Equal(QuoteType.Single, quoteType)
        | _ -> failwith "Unexpected expressions list"

    [<Fact>]
    let ``Can parse double quoted string`` () =
        parse "\"A double quoted string\"" |> function
        | [ Expr.Str(str, quoteType, _) ] -> 
            Assert.Equal("A double quoted string", str)
            Assert.Equal(QuoteType.Double, quoteType)
        | _ -> failwith "Unexpected expressions list"

    [<Fact>]
    let ``Can parse unary number``() =
        parse "-123.456" |> function
        | [ Expr.UnaryOperator(op, Expr.Num(num, _), _) ] -> 
            Assert.Equal(Op.Minus, op)
            Assert.Equal(123.456, num)
        | _ -> failwith "Unexpected expressions list"

    [<Fact>]
    let ``Can parse simple binary plus``() =
        parse "34 + 856" |> function
        | [ Expr.BinaryOperator(Expr.Num(lhs, _), op, Expr.Num(rhs, _), _) ] ->
            Assert.Equal(34.0, lhs)
            Assert.Equal(Op.Plus, op)
            Assert.Equal(856.0, rhs)
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
            ]-> Assert.Equal("average", name)                
        | _ as ls -> failwith ("Unexpected expression list: " + (ls.ToString()))