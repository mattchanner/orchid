namespace Orchid

open System.Collections.Generic
open System.IO
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
            Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), 
            Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location))

    let parse str =
        let result = Parser.parseString str environment
        if result.Errors.Length > 0 then 
            result.Errors 
            |> List.map (fun x -> sprintf "[Code %i]: %s -> %A" (x.Code) (x.Message) (x.Token))
            |> List.reduce (+)
            |> failwith
        else
            result.Expressions

    let parseAndEval str =
        parse str 
        |> List.item 0 
        |> eval environment 

    let parseAndEvalAll str =
        parse str 
        |> evalAll environment

    let evalBool str =
        let var = parseAndEvalAll str
        var.AsBoolValue(0).Value

    let evalNum str =
        let var = parseAndEvalAll str
        match var.AsDoubleValue(0) with
        | Some(value) -> value
        | None -> failwith <| sprintf "Unexpected var type returned %A" (var.TypeCode)

    
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

    type ``Basic evaluator tests``() =        

        [<Fact>]
        let ``Can evaluate list of numbers``() =
            let result = parseAndEval "{1, 2, 3, 4}"
            
            result.TypeCode |> should equal VarTypeCode.Number
            result.IsVector |> should be True
            result.Length |> should equal 4

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
            evalNum "1 + 2 * 3 / 4" |> should equal 2.5

        [<Fact>]
        let ``Can subtract within an if expression`` () =
            evalNum "if(1 - 2 > 0, 3 - 2, 5 - 1)" |> should equal 4.0

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

    type ``Multi step statements``() =
        
        [<Fact>]
        let ``Simple multi step evaluation``() =
            evalNum @"
                let x = 25;
                let y = 45;
                x + y
            " |> should equal 70.0

        [<Fact>]
        let ``Multi step test using array operations``() =
            evalNum @"
                # data
                let x = array(1.2, 2.3, 3.4, 5.6);

                let ave = Sum(x) / Size(x);
                let variance = Sum((x-ave)^2) / (Size(x)-1);
                let stdev = (variance) ^ 0.5;

                # return stdev as result
                stdev
            " |> should (equalWithin 0.001) 1.8786

        [<Fact>]
        let ``Can map array using anonymous expression``() =
            evalNum @"
                let data = {1, 2, 3, 4, 5, 6};
                let doubled = map(data, {item0 * 2});
                let ave = sum(doubled) / size(doubled);
                ave
            " |> should equal (42.0 / 6.0)

        [<Fact>]
        let ``Can filter array``() =
            let result = parseAndEvalAll @"
                let data = {1, 2, 3, 4, 5, 6};
                let evens = filter(data, {item % 2 = 0});
                evens"

            let da, _ = result.ToDoubleArrayWithKnockOut()
            Assert.Equal(da, [| 2.0; 4.0; 6.0 |] :> IEnumerable<float>)

            
            