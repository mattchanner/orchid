namespace Orchid.TypeSystem

open System
open System.Globalization
open System.Runtime.CompilerServices

open Orchid.Extensions

/// The Variables module provides a number of extensions to IVariable which are available to both F# code and other
/// .NET languages.  The way 
[<AutoOpen>]
[<Extension>]
module Variables =

    let (|DoubleValue|_|) (v: IVariable) = if v.Length <> 1 then None else v.AsDoubleValue(0)    
    let (|BoolValue|_|)   (v: IVariable) = if v.Length <> 1 then None else v.AsBoolValue(0)
            
    let private varEquals (var1: IVariable) (var2: IVariable) =
        if obj.ReferenceEquals(var1, var2) then true
        elif var1.Length <> var2.Length then false
        elif var1.TypeCode <> var2.TypeCode then false else
        let len = var1.Length
        let rec compIdx idx =
            if idx >= len then true else
            if var1.IsIncluded(idx) <> var2.IsIncluded(idx) then false else
            let typesMatch =
                match var1.TypeCode with
                | VarTypeCode.Bool   -> var1.AsBoolValue(idx)   = var2.AsBoolValue(idx)
                | VarTypeCode.Number -> var1.AsDoubleValue(idx) = var2.AsDoubleValue(idx)
                | _ -> var1.AsStringValue(idx) = var2.AsStringValue(idx)
            if not typesMatch then false
            else compIdx (idx + 1)
        compIdx 0

    let private compareVars (var1: IVariable) (var2: IVariable) =
        if var1.TypeCode <> var2.TypeCode then -1 else
        if var1.Length <> var2.Length then -1 else
        if var1.Length = 1 && var2.Length = 1 then
            match var1.TypeCode with
            | VarTypeCode.Bool   -> var1.AsBoolValue(0).Value.CompareTo(var2.AsBoolValue(0).Value)
            | VarTypeCode.String -> var1.AsStringValue(0).CompareTo(var2.AsStringValue(0))
            | VarTypeCode.Number -> var1.AsDoubleValue(0).Value.CompareTo(var2.AsDoubleValue(0).Value)
            |_ -> -1
        elif var1.Equals(var2) then 0 else -1

    let rec private createVariable (tc: VarTypeCode) // Type code of variable
        (ko: int[])                             // Knock out array
        (toStr: int -> string)                  // function used to convert the variable to a string at the given index
        (toDbl: int -> double option)           // function used to convert the variable to a double option at the given index
        (toBool: int -> bool option) =          // function used to convert the variable to a bool option at the given index

        let len = ko.Length

        // throws an IndexOutOfRangeException when index is 0 < index < range, otherwise execute f.
        let whenInRange (index, range) f = if index < range then f() else raise (IndexOutOfRangeException())

        { new IVariable with 
            member x.Length                     = len
            member x.TypeCode                   = tc
            member x.KoState(index: int)        = whenInRange (index, len) (fun () -> ko.[index]) 
            member x.IsExcluded(index: int)     = whenInRange (index, len) (fun () -> ko.[index] > 0)
            member x.IsIncluded(index: int)     = whenInRange (index, len) (fun () -> ko.[index] = 0)
            member x.IsAutoExcluded(index: int) = whenInRange (index, len) (fun () -> ko.[index] > 1)
            member x.IsError                    = tc = VarTypeCode.Error
            member x.IsScalar                   = len = 1
            member x.IsVector                   = len > 1
            member x.AsStringValue(index:int)   = whenInRange (index, len) (fun () -> toStr(index))
            member x.AsDoubleValue(index:int)   = whenInRange (index, len) (fun () -> toDbl(index))
            member x.AsBoolValue(index:int)     = whenInRange (index, len) (fun () -> toBool(index))
            member x.CompareTo(other:IVariable) = compareVars x other
            member x.Equals(other:IVariable)    = varEquals x other 
            member x.VariableAt(index: int) =
                // simple optimisation - return this instance when variable is a scalar and index = 0
                if index = 0 && len = 1 then 
                    x
                else
                    // capture requested index, as the new variable is just a view onto the index of this instance
                    let toStrScalar  = fun idx -> whenInRange (idx, 1) (fun () -> toStr(index))
                    let toDblScalar  = fun idx -> whenInRange (idx, 1) (fun () -> toDbl(index))
                    let toBoolScalar = fun idx -> whenInRange (idx, 1) (fun () -> toBool(index))
                    createVariable tc [|ko.[index]|] toStrScalar toDblScalar toBoolScalar }

    /// Makes a strongly typed variable which itself represents a sequence of 1 or more variables.
    /// This prevents the coercion of mixed types into strings.
    /// The constructed variable acts as a view onto each underlying variable, so indexed operations
    /// are normalized in order for correct access to the indexed value.
    /// e.g.
    /// A list var composed of the following:
    /// {1, 2, 3} | { a, b, c} | { true, false}
    /// Would produce the following array slots:
    /// (0, 2) | (3, 5) | (6, 7)
    /// A call to index 4 on the list variable would result in variable 2 being used as 4 falls within
    /// the range of this slot.
    /// The index is then normalised by subtracting the lower bounds of the slot from the index, e.g.
    /// 4 - 3, producing a normalised index of 1.
    /// The variable at this slot is then delegated to, passing the norm index to the requested operation
    let createListVars (vars: #seq<IVariable>) =

        let varArr = Array.ofSeq vars

        // generate a size based on the sum of all the variables
        let len = varArr 
                  |> Array.map (fun x -> x.Length) 
                  |> Array.fold (+) 0

        // Generate a list of slots containing the index ranges for each variable
        let makeSlots (vars:IVariable[]) =
            let rec loop idx accIdx acc = 
                if idx >= vars.Length then acc else
                let var = vars.[idx]
                loop (idx + 1) (accIdx + var.Length) ((accIdx, accIdx + var.Length, var) :: acc)
            (loop 0 0 []) |> List.rev

        // This function retrieves the variable within the list where index falls within the slot range.
        // The normalized index is also returned as part of the resulting tuple
        // e.g. slots = (0, 3) | (4, 6), index = 5 would return variable 2
        let varAtIndex slots index =
            let slot = List.tryFind (fun (singleSlot: int * int * IVariable) -> 
                match singleSlot with
                | (lower, upper, var) when index >= lower && index < upper -> true
                | _ -> false) slots
            match slot with
            | Some(lower, upper, var) -> 
                let normIdx = index - lower
                (var, normIdx)
            | None -> raise (IndexOutOfRangeException())

        // Normalizes the index based on the slots list, then executes f against the variable and index
        let normf slots index f = (varAtIndex slots index) |> f

        // Generate a type code based on each variable
        let typeCode =
            let rec loop idx tc =
                if idx = 0 then loop (idx + 1) (varArr.[idx].TypeCode) else
                if idx >= varArr.Length then tc else
                let var = varArr.[idx]
                if var.TypeCode <> tc then VarTypeCode.Mixed else 
                loop (idx + 1) tc
            loop 0 VarTypeCode.Bool

        let slots = makeSlots varArr

        { new IVariable with
            member x.Length                     = len
            member x.TypeCode                   = typeCode
            member x.KoState(index: int)        = normf slots index (fun (var, ni) -> var.KoState(ni))
            member x.IsExcluded(index: int)     = normf slots index (fun (var, ni) -> var.IsExcluded(ni))
            member x.IsIncluded(index: int)     = normf slots index (fun (var, ni) -> var.IsIncluded(ni))
            member x.IsAutoExcluded(index: int) = normf slots index (fun (var, ni) -> var.IsAutoExcluded(ni))
            member x.IsError                    = typeCode = VarTypeCode.Error
            member x.IsScalar                   = len = 1
            member x.IsVector                   = len > 1
            member x.AsStringValue(index:int)   = normf slots index (fun (var, ni) -> var.AsStringValue(ni))
            member x.AsDoubleValue(index:int)   = normf slots index (fun (var, ni) -> var.AsDoubleValue(ni))
            member x.AsBoolValue(index:int)     = normf slots index (fun (var, ni) -> var.AsBoolValue(ni))
            member x.CompareTo(other:IVariable) = compareVars x other
            member x.Equals(other:IVariable)    = varEquals x other 
            member x.VariableAt(index: int)     =
                let var, normIndex = varAtIndex slots index
                let toStr  = fun idx -> if idx = 0 then var.AsStringValue(normIndex) else raise (IndexOutOfRangeException())
                let toBool = fun idx -> if idx = 0 then var.AsBoolValue(normIndex)   else raise (IndexOutOfRangeException())
                let toNum  = fun idx -> if idx = 0 then var.AsDoubleValue(normIndex) else raise (IndexOutOfRangeException())
                createVariable var.TypeCode [|var.KoState(normIndex)|] toStr toNum toBool }

    /// Creates a numberic based variable
    let createNum (dbl: double) (ko: int) =
        createVariable 
            VarTypeCode.Number
            [|ko|]
            (fun idx -> dbl.ToString(CultureInfo.InvariantCulture))
            (fun idx -> Some(dbl))
            (fun idx -> Some(dbl <> 0.0))

    /// Creates a number array based variable
    let createNumVector (d: double[]) (ko: int[]) =
        let len = d.Length
        createVariable
            VarTypeCode.Number
            ko
            (fun idx -> d.[idx].ToString(CultureInfo.InvariantCulture))
            (fun idx -> Some(d.[idx]))
            (fun idx -> None) // No conversion from num to bool here - potentially, this could be used in a truthy way (non 0 == true)

    /// Creates a bool based variable
    let createBool (b: bool) (ko: int) =
        createVariable VarTypeCode.Bool [|ko|] (fun idx -> b.ToString(CultureInfo.InvariantCulture)) (fun idx -> None) (fun idx -> Some(b))

    /// Creates a bool array based variable
    let createBoolVector (b: bool[]) (ko: int[]) =
        let len = b.Length
        createVariable
            VarTypeCode.Bool
            ko
            (fun idx -> b.[idx].ToString(CultureInfo.InvariantCulture))
            (fun idx -> None)
            (fun idx -> Some(b.[idx]))

    /// Creates a string based variable
    let createString (s: string) (ko: int) =
        let maybeDouble = lazy(s.MaybeDouble())
        let maybeBool = lazy(s.MaybeBoolean())
        createVariable
            VarTypeCode.String
            [|ko|]
            (fun idx -> s)
            (fun idx -> maybeDouble.Force())
            (fun idx -> maybeBool.Force())

    /// Creates a string array based variable
    let createStringVector (s: string[]) (ko: int[]) =
        let len = s.Length
        createVariable
            VarTypeCode.String
            ko
            (fun idx -> s.[idx])
            (fun idx -> s.[idx].MaybeDouble())
            (fun idx -> s.[idx].MaybeBoolean())

    /// Creates a variable which represents an error condition
    let createError (reason: string) =
        createVariable
            VarTypeCode.Error
            [|2|]
            (fun idx -> if idx = 0 then reason else raise (IndexOutOfRangeException()))
            (fun idx -> if idx = 0 then None else raise (IndexOutOfRangeException()))
            (fun idx -> if idx = 0 then None else raise (IndexOutOfRangeException()))

    /// Returns a string array representation of the underlying variable, along with
    /// it's corresponding knockout array
    [<Extension>]
    [<CompiledName("ToStringArrayWithKnockOut")>]
    let toStringArrayWithKnockOut (x: IVariable) =  
        let ra, ko = new ResizeArray<string>(), new ResizeArray<int>()
        for i in 0 .. (x.Length - 1) do
            ra.Add(x.AsStringValue(i))
            ko.Add(x.KoState(i))
        (ra.ToArray(), ko.ToArray())

    /// Returns a double array representation of the underlying variable, along with
    /// it's corresponding knockout array. Note that any non numeric values will be filtered
    /// out of the output array, which could therefore result in an output array smaller than
    /// the size of the variable
    [<Extension>]
    [<CompiledName("ToDoubleArrayWithKnockOut")>]
    let toDoubleArrayWithKnockOut (x: IVariable)  =  
        let ra, ko = new ResizeArray<double>(), new ResizeArray<int>()
        for i in 0 .. (x.Length - 1) do
            match x.AsDoubleValue(i) with
            | Some(d) -> 
                ra.Add(d)
                ko.Add(x.KoState(i))
            | None -> 
                ra.Add(nan)
                ko.Add(2)
        (ra.ToArray(), ko.ToArray())

    /// Returns a double  array representation of the given variable. Any non numeric values will
    /// be removed
    [<Extension>]
    [<CompiledName("ToDoubleArray")>]
    let toDoubleArray (x: IVariable) =
        let ra = ResizeArray<double>()
        for i in 0 .. (x.Length - 1) do
            match x.AsDoubleValue i with
            | Some(value) -> ra.Add(value)
            | _ -> ()
        ra.ToArray()

    /// Returns a bool array representation of the given variable. Any non boolean values will
    /// be removed
    [<Extension>]
    [<CompiledName("ToBoolArray")>]
    let toBoolArray (x: IVariable) =
        let ra = ResizeArray<bool>()
        for i in 0 .. (x.Length - 1) do
            match x.AsBoolValue i with
            | Some(value) -> ra.Add(value)
            | _ -> ()
        ra.ToArray()

    /// Returns a string array representation of the given variable
    [<Extension>]
    [<CompiledName("ToStringArray")>]
    let toStringArray (x: IVariable) =
        let ra = ResizeArray<string>()
        for i in 0 .. (x.Length - 1) do
            ra.Add (x.AsStringValue i)
        ra.ToArray()

    /// Returns a boolean array representation of the underlying variable, along with
    /// it's corresponding knockout array. Note that any non numeric values will be filtered
    /// out of the output array, which could therefore result in an output array smaller than
    /// the size of the variable
    [<Extension>]
    [<CompiledName("ToBoolArrayWithKnockOut")>]
    let toBoolArrayWithKnockOut (x: IVariable) =  
        let ra, ko = new ResizeArray<bool>(), new ResizeArray<int>()
        for i in 0 .. (x.Length - 1) do
            match x.AsBoolValue(i) with
            | Some(d) -> 
                ra.Add(d)
                ko.Add(x.KoState(i))
            | None -> ()
        (ra.ToArray(), ko.ToArray())

    let varsToStringArrayWithKo(vars: seq<IVariable>) =
        let ra = new ResizeArray<string>()
        let ko = new ResizeArray<int>()
        for var in vars do
            let values, kos = toStringArrayWithKnockOut var
            ra.AddRange(values)
            ko.AddRange(kos)
        (ra.ToArray(), ko.ToArray())

    /// Creates a tuple containnig 
    let varsToDoubleArrayWithKo(vars: seq<IVariable>) =
        let ra = new ResizeArray<double>()
        let ko = new ResizeArray<int>()
        for var in vars do
            let values, kos = toDoubleArrayWithKnockOut var
            ra.AddRange(values)
            ko.AddRange(kos)
        (ra.ToArray(), ko.ToArray())

    let varsToBoolArrayWithKo(vars: seq<IVariable>) =
        let ra = new ResizeArray<bool>()
        let ko = new ResizeArray<int>()
        for var in vars do
            let values, kos = toBoolArrayWithKnockOut var
            ra.AddRange(values)
            ko.AddRange(kos)
        (ra.ToArray(), ko.ToArray())

    /// F# specific extension
    type IVariable with

        /// Returns a string array representation of the underlying variable, along with
        /// it's corresponding knockout array
        member x.ToStringArrayWithKnockOut () = toStringArrayWithKnockOut x

        /// Returns a double array representation of the underlying variable, along with
        /// it's corresponding knockout array. Note that any non numeric values will be filtered
        /// out of the output array, which could therefore result in an output array smaller than
        /// the size of the variable
        member x.ToDoubleArrayWithKnockOut () = toDoubleArrayWithKnockOut x

        /// Returns a boolean array representation of the underlying variable, along with
        /// it's corresponding knockout array. Note that any non numeric values will be filtered
        /// out of the output array, which could therefore result in an output array smaller than
        /// the size of the variable
        member x.ToBoolArrayWithKnockOut () =  toBoolArrayWithKnockOut x

        /// Factory for creating operators
        static member private mkOp (var1: IVariable) (var2: IVariable) (op: double -> double -> double) (opName: string) =
            // scalar * scalar
            if var1.Length = 1 && var2.Length = 1 then
                match var1.AsDoubleValue(0), var2.AsDoubleValue(0) with
                | Some(lhs), Some(rhs) ->  createNum (op lhs rhs) (max (var1.KoState(0)) (var2.KoState(0)))
                | _ -> createError(sprintf "Unsupported types for %s" opName)

            // vector * scalar - allows {1, 2, 3} * 2 -> {2, 4, 6}
            elif var1.Length > 1 && var2.Length = 1 then
                match var2.AsDoubleValue(0) with
                | Some(rhs) ->
                    let multiplier =
                        seq {
                            for i in 0 .. (var1.Length - 1) do
                                match var1.AsDoubleValue(i) with
                                | Some(lhs) -> yield (op lhs rhs), var1.KoState(i)
                                | None -> yield (nan, 2) 
                        }
                    let (values, ko) = (multiplier |> Seq.toArray |> Array.unzip)
                    createNumVector values ko
                | None -> createError(sprintf "Unsupported types for %s" opName)
            elif var1.Length = var2.Length then
                let data1, ko1 = var1.ToDoubleArrayWithKnockOut()
                let data2, ko2 = var2.ToDoubleArrayWithKnockOut()
                let res = Array.map2 (fun x y -> op x y) data1 data2
                let ko3 = Array.map2 max ko1 ko2
                createNumVector res ko3
            else
                createError(sprintf "Length mismatch for %s" opName)

        static member private mkLogicalOp 
            (var1: IVariable) 
            (var2: IVariable) 
            (op: bool -> bool -> bool) 
            (opName: string) =

            if var1.Length = 1 && var2.Length = 1 then
                match var1.AsBoolValue(0), var2.AsBoolValue(0) with
                | Some(lhs), Some(rhs) -> createBool (op lhs rhs) (max (var1.KoState(0)) (var2.KoState(0)))
                | _ -> createError(sprintf "Unsupported types for %s" opName)
            else
                createError(sprintf "Length mismatch for %s" opName)

        /// Adds the given variable to this instance, returning the result as a new variable
        member x.Add(other: IVariable) = 
            if (x.Length = other.Length) && (other.Length = 1) then
                match x.AsDoubleValue(0), other.AsDoubleValue(0) with
                | Some(lhs), Some(rhs) -> createNum (lhs + rhs) (max (x.KoState(0)) (other.KoState(0)))
                | _ -> createString(x.AsStringValue(0) + other.AsStringValue(0)) (max (x.KoState(0)) (other.KoState(0)))
            else IVariable.mkOp x other (+) "plus"

        /// Subtracts the given variable from this instance, returning the result as a new variable
        member x.Subtract(other: IVariable) = IVariable.mkOp x other (-) "minus"
        
        /// Divides this instance by the one provided, returning the result as a new variable
        member x.DivideBy(other: IVariable) = IVariable.mkOp x other (/) "divide"
        
        /// Multiplies this instance by the one provided, returning the result as a new variable
        member x.MultiplyBy(other: IVariable) = IVariable.mkOp x other (*) "multiply"
        
        /// Returns the result of performing a modulus of this instance with the one given
        member x.Modulus(other: IVariable) = IVariable.mkOp x other (%) "modulus"
        
        /// Returns the result of performing a power of this instance with the one given
        member x.Pow(other: IVariable) = IVariable.mkOp x other ( ** ) "power"
        
        /// And's this variable with the one provided, assumes that both variables can be converted
        /// to a boolean, otherwise an exception is raised
        member x.AndWith(other: IVariable) = IVariable.mkLogicalOp x other (&&) "and"
        
        /// Or's this variable with the one provided, assumes that both variables can be converted
        /// to a boolean, otherwise an exception is raised
        member x.OrWith(other: IVariable) = IVariable.mkLogicalOp x other (||) "or"

        /// To string representation of the variable
        member x.ToString(fmt: IFormatProvider) =
            let builder = System.Text.StringBuilder()
            let vals, ko = x.ToStringArrayWithKnockOut()
            for (s, ko) in Array.zip vals ko do
                builder.Append(s) |> ignore
                if ko > 0 then builder.Append("*") |> ignore
                builder.Append(",") |> ignore
            let mutable res = builder.ToString()
            if res.EndsWith(",") then res <- res.Substring(0, res.Length - 1)
            res

    /// Returns a new variable representing the reverse of the input variable
    [<Extension>]
    [<CompiledName("Reverse")>]
    let rev (input: IVariable) =
        
        // Reverse the given index based on the input variable's data length
        let revIndex index = input.Length - index - 1

        // Create a new implementation of a variable which is a reversed view of the input variable
        { new IVariable with 
            member x.Equals (other: IVariable) = varEquals x other
            member x.CompareTo (other: IVariable) = compareVars x other
            member x.TypeCode = input.TypeCode
            member x.Length = input.Length
            member x.IsExcluded (index: int) = input.IsExcluded (revIndex index)
            member x.IsIncluded (index: int) = input.IsIncluded (revIndex index)
            member x.KoState (index: int) = input.KoState (revIndex index)
            member x.IsError = input.IsError
            member x.IsAutoExcluded (index: int) = input.IsAutoExcluded (revIndex index)
            member x.IsScalar = input.IsScalar
            member x.IsVector = input.IsVector
            member x.AsStringValue (index:int) = input.AsStringValue (revIndex index)
            member x.AsDoubleValue (index:int) = input.AsDoubleValue (revIndex index)
            member x.AsBoolValue (index:int) = input.AsBoolValue (revIndex index)
            member x.VariableAt (index: int) = input.VariableAt (revIndex index) }


    [<Extension>]
    [<CompiledName("ForEach")>]
    let iter (x: IVariable) (fn: System.Action<IVariable>) =
        for i in 0 .. (x.Length - 1) do
            fn.Invoke(x.VariableAt i)

    [<Extension>]
    [<CompiledName("Select")>]
    let map<'a> (x: IVariable) (fn: System.Func<IVariable, 'a>) =
        seq { for i in 0 .. (x.Length - 1)  -> fn.Invoke (x.VariableAt i) }

    [<CompiledName("ToSeq")>]
    let toSeq (x: IVariable) = seq { for i in 0 .. (x.Length - 1)  -> x.VariableAt i }
    
    [<CompiledName("ToArray")>]
    let toArray (x: IVariable) = [| for i in 0 .. (x.Length - 1)  -> x.VariableAt i |]

    /// Returns a list of variables 
    [<CompiledName("ToList")>]
    let toList (x: IVariable) = [ for i in 0 .. (x.Length - 1)  -> x.VariableAt i ]

    [<Extension>]
    let ToString(x: IVariable, fmt: IFormatProvider) = x.ToString(fmt)

    /// Adds the given variable to this instance, returning the result as a new variable
    [<Extension>]
    [<CompiledName("Add")>]
    let add(x: IVariable, other: IVariable) = x.Add other

    /// Subtracts the given variable from this instance, returning the result as a new variable
    [<Extension>]
    [<CompiledName("Subtract")>]
    let subtract(x: IVariable, other: IVariable) = x.Subtract other
        
    /// Divides this instance by the one provided, returning the result as a new variable
    [<Extension>]
    [<CompiledName("DivideBy")>]
    let divideBy(x: IVariable, other: IVariable) = x.DivideBy other
        
    /// Multiplies this instance by the one provided, returning the result as a new variable
    [<Extension>]
    [<CompiledName("MultiplyBy")>]
    let multiplyBy(x: IVariable, other: IVariable) = x.MultiplyBy other
        
    /// Returns the result of performing a modulus of this instance with the one given
    [<Extension>]
    [<CompiledName("Modulus")>]
    let modulus(x: IVariable, other: IVariable) = x.Modulus other
        
    /// Returns the result of performing a power of this instance with the one given
    [<Extension>]
    [<CompiledName("Pow")>]
    let pow(x: IVariable, other: IVariable) = x.Pow other
        
    /// And's this variable with the one provided, assumes that both variables can be converted
    /// to a boolean, otherwise an exception is raised
    [<Extension>]
    [<CompiledName("AndWith")>]
    let andWith(x: IVariable, other: IVariable) = x.AndWith other
        
    /// Or's this variable with the one provided, assumes that both variables can be converted
    /// to a boolean, otherwise an exception is raised
    [<Extension>]
    [<CompiledName("OrWith")>]
    let orWith(x: IVariable, other: IVariable) = x.OrWith other