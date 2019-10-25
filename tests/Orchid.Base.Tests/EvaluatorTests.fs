namespace Orchid

open System.Collections.Generic
open System.Reflection

open Orchid.Expressions
open Orchid.Expressions.Evaluator
open Orchid.Runtime
open Orchid.TypeSystem

open Xunit

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
        Assert.Equal(VarTypeCode.Bool, result.TypeCode)
        Assert.True(result.IsVector)
        Assert.Equal(4, result.Length)

        let booleans, _ = result.ToBoolArrayWithKnockOut()
        let expected  = [|true; false; false; true|] 
        
        Assert.Equal(expected, booleans :> IEnumerable<bool>)
        
        