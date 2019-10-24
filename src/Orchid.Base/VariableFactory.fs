namespace Orchid.TypeSystem

open System
open System.Collections.Generic

open Orchid.Extensions

type VariableFactory() =

    /// Constructs a new error variable from an exception
    static member MakeError (e: exn) = 
        let rec underlyingExn (e:exn) =
            match e.InnerException with
            | null -> e
            | _ as inner -> underlyingExn inner
        let cause = underlyingExn e        
        createError(cause.Message)

    /// Constructs a new error variable
    static member MakeError (reason: string) = createError(reason)

    /// Constructs a new string variable
    static member MakeVariable (s:string) = createString s 0

    /// Constructs a new string variable with the given knock out array
    static member MakeVariable (s:string, ko: int) = createString s ko

    /// Constructs a new double variable
    static member MakeVariable (d: double) = createNum d 0

    /// Constructs a new double variable with the given knock out array
    static member MakeVariable (d: double, ko: int) = createNum d ko

    /// Constructs a new boolean variable
    static member MakeVariable (b: bool) = createBool b 0

    /// Constructs a new boolean variable with the given knock out array
    static member MakeVariable (b: bool, ko: int) = createBool b ko

    /// Constructs a new string array variable
    static member MakeVariable (sa: string[]) = createStringVector sa (Array.create sa.Length 0)

    /// Constructs a new string array variable including the given knock out array
    static member MakeVariable (sa: string[], kos: int[]) = createStringVector sa kos

    /// Constructs a new double array variable
    static member MakeVariable (da: float[]) = createNumVector da (Array.create da.Length 0)

    /// Constructs a new double array variable with the given knock out states
    static member MakeVariable (da: float[], kos: int[]) = createNumVector da kos

    /// Constructs a new boolean array variable
    static member MakeVariable (ba: bool[]) = createBoolVector ba (Array.create ba.Length 0)

    /// Constructs a new boolean array variable with the given knock out states
    static member MakeVariable (ba: bool[], kos: int[]) = createBoolVector ba kos

    /// Constructs a new variable from a sequence of inputs. If all types in the sequence
    /// are of the same type, the resulting variable will be a vector type.  If the sequence
    /// contains a mixture of variables, the result will be a list type, with the type of
    /// each input variable preserved.
    static member MakeVariable (vars: #seq<IVariable>) =

        let StringSlot, BoolSlot, DoubleSlot, ErrorSlot, MixedSlot = 0, 1, 2, 3, 4

        // Freeze the var seq in order to prevent performance issues which may be caused from iterating over it multiple times
        let frozenVars = Seq.cache vars

        // map each variable to create a tuple list of type * var    
        let makeTypeInfo (vars: seq<IVariable>) = vars |> Seq.map (fun v -> (v.TypeCode, v))

        // partitions (type * var) into a dictionary<type, list<var>>
        let partition (types: seq<VarTypeCode * IVariable>) =
            let partitions = new Dictionary<VarTypeCode, ResizeArray<IVariable>>()
            for (t, v) in types do
                let arr = partitions.GetOrInit(t, (fun _ -> new ResizeArray<IVariable>()))
                arr.Add(v)
            partitions

        // creates a tuple containing the number of types found (strings, booleans, doubles, errors | unknown)
        let countTypes (dict:IDictionary<VarTypeCode, ResizeArray<IVariable>>) =
            let types = [|0; 0; 0; 0; 0|]
            for k in dict.Keys do
                match k with
                | VarTypeCode.String -> types.[StringSlot] <- types.[StringSlot] + 1
                | VarTypeCode.Bool   -> types.[BoolSlot]   <- types.[BoolSlot]   + 1
                | VarTypeCode.Number -> types.[DoubleSlot] <- types.[DoubleSlot] + 1
                | VarTypeCode.Error  -> types.[ErrorSlot]  <- types.[ErrorSlot]  + 1
                | VarTypeCode.Mixed  -> types.[MixedSlot]  <- types.[MixedSlot]  + 1
                | _ -> raise (ArgumentOutOfRangeException())

            // Return results as a tuple
            (types.[StringSlot], types.[BoolSlot], types.[DoubleSlot], types.[ErrorSlot], types.[MixedSlot])
                        
        let counted = frozenVars |> makeTypeInfo |> partition |> countTypes

        // Map vars into a tuple of type * var, split tuple into a dict of type * var[] and generate
        // a tuple containing the number of distinct types found
        match counted with
        | (_, 0, 0, 0, 0) -> vars |> Variables.varsToStringArrayWithKo |> VariableFactory.MakeVariable
        | (0, _, 0, 0, 0) -> vars |> Variables.varsToBoolArrayWithKo   |> VariableFactory.MakeVariable
        | (0, 0, _, 0, 0) -> vars |> Variables.varsToDoubleArrayWithKo |> VariableFactory.MakeVariable
        | (0, 0, 0, _, 0) -> Seq.head vars
        | _ -> Variables.createListVars vars // mixed input, create mixed view on data