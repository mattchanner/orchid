namespace Orchid

open System.Collections.Generic
open System.Reflection

open Orchid.Expressions
open Orchid.Expressions.Evaluator
open Orchid.Runtime
open Orchid.TypeSystem

open Xunit
open FsUnit.Xunit

module EvaluatorTests =

    let environment = 
        EnvironmentSetup.SetupFromPaths(
            Assembly.GetExecutingAssembly().Location, 
            Assembly.GetExecutingAssembly().Location)

    let parse str =
        let result = Parser.parseString str environment
        result.Expressions

    let parseAndEval str =
        parse str 
        |> List.item 0 
        |> eval environment 

    let parseAndEvalAll str =
        parse str 
        |> evalAll environment

    let evalBool str =
        let var = parseAndEval str
        var.AsBoolValue(0).Value

    let evalNum str =
        let var = parseAndEval str
        var.AsDoubleValue(0).Value

    type ``Basic evaluator tests``() =        

        [<Fact>]
        let ``Can evaluate list of numbers``() =
            let result = parseAndEval "{1, 2, 3, 4}"
            Assert.Equal(VarTypeCode.Number, result.TypeCode)
            Assert.True(result.IsVector)
            Assert.Equal(4, result.Length)

            let numbers, _ = result.ToDoubleArrayWithKnockOut()
            let expected  = [|1.0; 2.0; 3.0; 4.0|] 
        
            Assert.Equal(expected, numbers :> IEnumerable<float>)

        [<Fact>]
        let ``Can evaluate list of bools``() =
            let result = parseAndEval "{true, false, false, true}"

            result.TypeCode |> should equal VarTypeCode.Bool
            result.IsVector |> should be True
            result.Length |> should equal 4

            let booleans, _ = result.ToBoolArrayWithKnockOut()
            let expected  = [|true; false; false; true|] 
        
            Assert.Equal(expected, booleans :> IEnumerable<bool>)
        
        [<Fact>]
        let ``Can parse and eval operators with different precedence``() =
            let result = parseAndEval "1 + 2 * 3 / 4"
            match result.AsDoubleValue(0) with
            | Some(value) -> value |> should equal 2.5
            | None -> failwith ("Unexpected double value of " + result.ToString())

        [<Fact>]
        let ``Can subtract within an if expression`` () =
            let var = parseAndEval "if(1 - 2 > 0, 3 - 2, 5 - 1)"
            match var.AsDoubleValue(0) with
            | Some(value) -> value |> should equal 4.0
            | r -> failwith ("Unexpected variable returned " + r.Value.ToString())

        [<Fact>]
        let ``Can eval logical and operator on valid bool expressions``() =
            evalBool "3 > 2 && 4 > 2" |> should be True
            evalBool "3 > 2 && 4 < 2" |> should be False
            evalBool "((3 > 2) && (4 > 2)) && 3 > 2" |> should be True

        [<Fact>]
        let ``Can eval logical or operator on valid bool expressions``() =
            evalBool "3 > 2 || 1 > 2" |> should be True
            evalBool "3 > 2 || 4 < 2" |> should be True
            evalBool "((3 > 7) || (4 > 5)) || 3 > 2" |> Assert.True
            
        [<Fact>]
        let ``Can override operator precedence using parens``() =
            evalNum "(1 + 2) * 3" |> should equal 9.0
            evalNum "1 + 2 * 3"   |> should equal 7.0

    type ``Operator tests``() =

        [<Fact>]
        let ``> test``() =
            evalBool "3 > 2" |> should be True
            evalBool "2 > 3" |> should be False

        [<Fact>]
        let ``>= test``() =
            evalBool "2 >= 2" |> should be True
            evalBool "3 >= 2" |> should be True
            evalBool "3 >= 5" |> should be False

        [<Fact>]
        let ``< test``() =
            evalBool "1 < 2" |> should be True
            evalBool "1 > 2" |> should be False

        [<Fact>]
        let ``<= test``() =
            evalBool "2.0 <= 2.0" |> should be True
            evalBool "1.2 <= 2" |> should be True
            evalBool "3 <= 2.999" |> should be False

        [<Fact>]
        let ``+ test``() =
            evalNum "1.5 + 1" |> should equal 2.5

        [<Fact>]
        let ``- test``() =
            evalNum "1.5 - 1" |> should equal 0.5

        [<Fact>]
        let ``* test``() =
            evalNum "1.5 * 1" |> should equal 1.5
            evalNum "2 * 2" |> should equal 4.0

        [<Fact>]
        let ``/ test``() =
            evalNum "3.5 / 2.0" |> should equal 1.75

        [<Fact>]
        let ``% test``() =
            evalNum "10 % 3" |> should equal (10.0 % 3.0)

        [<Fact>]
        let ``^ test``() =
            evalNum "16 ^ 4.2" |> should equal (16.0 ** 4.2)