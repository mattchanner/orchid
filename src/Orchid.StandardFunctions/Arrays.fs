namespace Orchid.Lib.Functions

open System.Collections.Generic
open System.Linq

open Orchid
open Orchid.Expressions
open Orchid.Runtime
open Orchid.TypeSystem
open Orchid.TypeSystem.Variables

module public ArrayFunctions =

    /// Returns a new item which is the reverse of the input
    [<Function("Array Functions", "Reverses a source array")>]
    let Reverse(input: IVariable) = Variables.rev input

    /// Creates a new unique ordered list of variables based on the input
    [<Function("Array Functions", "Returns a unique list of values with order preserved")>]
    let DistinctList(input: IVariable) =
        match input.TypeCode with
        | VarTypeCode.Number -> 
            let dbls, kos = input.ToDoubleArrayWithKnockOut()            
            let (uniques: double[]) = Orchid.Seq.unique dbls
            VariableFactory.MakeVariable(uniques)
        | VarTypeCode.String ->
            let sas, kos = input.ToStringArrayWithKnockOut()
            let (uniques: string[]) = Orchid.Seq.unique sas
            VariableFactory.MakeVariable(uniques)
        | VarTypeCode.Bool ->
            let bools, kos = input.ToBoolArrayWithKnockOut()
            let (uniques: bool[]) = Orchid.Seq.unique bools
            VariableFactory.MakeVariable(uniques)
        | _ -> invalidOp("Unsupported data type for array function")

    /// Returns the item at the given 1 based index
    [<Function("Array functions", "Returns the nth item (1 based) from the source data")>]
    let ItemAt (input: IVariable) (index: int) = 
        let zeroBasedIdx = index - 1
        input.VariableAt(zeroBasedIdx)

    /// Slices the given list from the start index, with the given length
    [<Function("Array Functions", 
        "This function extracts one or more consecutive elements from the input array.  
         The startingIndex is the 1 based element to start the extraction from.  
         The number of elements to extract is defined by the length argument.")>]
    let Slice (input: IVariable) (startIndex: int) (length: int) =
        let slice s start len =
            s
            |> Seq.skip start 
            |> Seq.take len 
            |> Seq.toArray 

        if startIndex > (input.Length) then
            invalidArg "startIndex" "Starting index exceeds the number of elements in the array"
        elif startIndex < 1 then
            invalidArg "startIndex" "Starting index must be greater than 1 and <= the size of the array"

        let zeroIndex = startIndex - 1
        let normLen = 
            // cap the length to the end of the array
            if length + zeroIndex > input.Length 
            then input.Length - zeroIndex
            else length            

        match input.TypeCode with
        | VarTypeCode.Bool ->
            let bools, kos = input.ToBoolArrayWithKnockOut()
            let sb, skos = slice bools zeroIndex normLen, slice kos zeroIndex normLen
            VariableFactory.MakeVariable(sb, skos)
        | VarTypeCode.Number ->
            let nums, kos = input.ToDoubleArrayWithKnockOut()
            let sn, skos = slice nums zeroIndex normLen, slice kos zeroIndex normLen
            VariableFactory.MakeVariable(sn, skos)
        | _ ->
            let seq = seq { for i in zeroIndex .. (startIndex + normLen - 1) -> input.VariableAt(i) }
            VariableFactory.MakeVariable(seq)

    /// Sorts the input list
    [<Function("Array Functions", "Sorts the numerical input array in ascending order")>]
    let Sort (input: double[]) = Array.sort input

    /// Reverse sort
    [<Function("Array Functions", "Sorts the numerical input array in descending order")>]
    let RSort (input: double[]) = (Array.sort input ) |> Array.rev

    [<Function("Array Functions", "Sorts the numerical input array in ascending order if the 
    final argument is true, otherwise the array will be sorted in descending order", "Sort")>]
    let Sort2 (input: double[], ascending: bool) = if ascending then Sort(input) else RSort(input)

    [<Function("Array Functions", "This function returns the (1 based) index of the 
    lookupValue from within the array argument")>]
    let IndexOf (array: IVariable, lookup: IVariable) =
        let sa = VariableConverter.ConvertTo<string[]> array false
        let lookupStr = VariableConverter.ConvertTo<string> lookup false
        let idx = System.Array.IndexOf(sa, lookupStr)
        if idx >= 0 then idx + 1 else idx

    [<Function("Array Functions", "A function to calculate a series of elements based on a starting 
    value and division factor. If the number of replicates > 1, the current value will be replicated in a block i.e. 
    a starting value of 100, with a dilution of 10, for 6 elements and 2 replicates would result in a return 
    list of 100,100,10,10,1,1")>]
    let Series (startValue: double,
                divisionFactor: double,
                numberOfElements: int,
                numberOfReplicates: int) =

        if numberOfElements < 1 then invalidArg "numberOfElements" "Value can be less than 1"
        if divisionFactor = 0.0 then invalidArg "divisionFactor" "Division factor can not be 0"

        let rec iter i rep prev (acc:double list): double list =
            if i > numberOfElements then acc else
            if rep = numberOfReplicates then
                let newVal = prev / divisionFactor
                iter (i + 1) 0 newVal (newVal::acc)
            else
                iter (i + 1) (rep + 1) prev (prev::acc)
        (iter 0 0 startValue []) |> List.rev |> List.toArray

    [<Function("Array Functions", "Creates a sequence of numbers beginning with the starting value 
    through to the end value, incrementing each step by the value given as the last argument. 
    Example : Step(2, 10, 2) will produce 
    2,4,6,8,10. If start > end, the list will be reversed: Step(10,2,2) = 10,8,6,4,2")>]
    let Step (start:double, end':double, increment:double) =
        if start = end' || increment = 0.0 then
            [|start|]
        else
            let rec collect s e next acc =
                if (s + next) > e then acc 
                else collect s e (next + increment) ((s + next)::acc)
            let rec collectBack s e next acc =
                if (s - next) < e then acc 
                else collectBack s e (next + increment) ((s - next)::acc)

            let ls = 
                if start < end' then collect start end' 0.0 []
                else collectBack start end' 0.0 []
            ls |> List.rev |> List.toArray

    [<Function("Array Functions", 
        "Creates a sequence of numbers beginning with the starting value 
        through to the end value. Example : Step(1, 5) will produce 
        1,2,3,4,5. If start > end, the list will be reversed: Step(5,1) = 5,4,3,2,1",
        "Step" (* XE Overload *))>]
    let Step1 (start:double, end':double) = Step(start, end', 1.0)