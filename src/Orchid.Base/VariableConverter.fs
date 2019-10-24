namespace Orchid.TypeSystem

open System
open System.Globalization

/// Common conversion functions for variables
module VariableConverter =

    /// Invariant culture used for formatting when the caller does not supply a format provider
    let private invariant:IFormatProvider = CultureInfo.InvariantCulture :> IFormatProvider

    let private booltype        = typeof<bool>
    let private stringtype      = typeof<string>
    let private doubletype      = typeof<double>
    let private inttype         = typeof<int>
    let private vartype         = typeof<IVariable>

    let private boolarraytype   = typeof<bool[]>
    let private stringarraytype = typeof<string[]>
    let private doublearraytype = typeof<double[]>
    let private intarraytype    = typeof<int[]>

    let private boolseqtype     = typeof<seq<bool>>
    let private stringseqtype   = typeof<seq<string>>
    let private doubleseqtype   = typeof<seq<double>>
    let private intseqtype      = typeof<seq<int>>

    /// Returns a value indicating whether the variable can be converted to the given type.
    let CanConvertToType (var: IVariable) (t: Type) =
        match t with
        | _ when vartype.IsAssignableFrom(t) -> true
        | _ when t = vartype         -> true
        | _ when t = booltype        -> var.TypeCode = VarTypeCode.Bool && var.Length = 1
        | _ when t = stringtype      -> var.Length = 1
        | _ when t = doubletype      -> var.Length = 1
        | _ when t = inttype         -> var.Length = 1
        | _ when t = boolarraytype   -> var.TypeCode = VarTypeCode.Bool
        | _ when t = stringarraytype -> true
        | _ when t = doublearraytype -> true
        | _ when t = intarraytype    -> true
        | _ when t = boolseqtype     -> var.TypeCode = VarTypeCode.Bool
        | _ when t = stringseqtype   -> true
        | _ when t = doubleseqtype   -> true
        | _ when t = intseqtype      -> true
        | _ -> false

    let private filterPoints (pts: 'T[]) (kos: int[]) =
        Array.zip pts kos
        |> Array.filter (fun (p, ko) -> ko = 0)
        |> Array.map (fun (p, _) -> p)        

    /// Converts the variable to an object of the request type.  This assumes that CanConvertToType
    /// reports true for the request type, otherwise an exception will be raised.
    let ConvertToType (var: IVariable) (t: Type) (removeKoPoints: bool) =
        match t with
        | _ when t = vartype || vartype.IsAssignableFrom(t) -> box var
        | _ when t = booltype   -> 
            match var.AsBoolValue(0) with
            | Some(b) -> box b
            | _ -> failwith "Unable to convert to boolean"
        | _ when t = stringtype -> box (var.AsStringValue(0))  
        | _ when t = doubletype -> 
            match var.AsDoubleValue(0) with
            | Some(d) -> box d
            | _ -> failwith "Unable to convert to double"
        | _ when t = inttype ->  
            match var.AsDoubleValue(0) with
            | Some(d) -> box (int d)
            | _ -> failwith "Unable to convert to int"
        | _ when t = boolarraytype -> 
            let bools, kos = var.ToBoolArrayWithKnockOut()
            if removeKoPoints then
                filterPoints bools kos |> box
            else
                box bools
        | _ when t = stringarraytype ->

            let strings, kos = var.ToStringArrayWithKnockOut()
            if removeKoPoints then filterPoints strings kos |> box
            else box strings            
        
        | _ when t = doublearraytype -> 
        
            let doubles, kos = var.ToDoubleArrayWithKnockOut()
            if removeKoPoints then filterPoints doubles kos |> box
            else box doubles
        
        | _ when t = intarraytype -> 
        
            let doubles, kos = var.ToDoubleArrayWithKnockOut()
            let ints = Array.map int doubles
            if removeKoPoints then filterPoints ints kos |> box
            else box ints
        
        | _ when t = boolseqtype ->

            let bools, kos = var.ToBoolArrayWithKnockOut()
            if removeKoPoints then filterPoints bools kos |> Seq.ofArray |> box
            else Seq.ofArray bools |> box
                            
        | _ when t = stringseqtype ->
            
            let strings, kos = var.ToStringArrayWithKnockOut()
            if removeKoPoints then filterPoints strings kos |> Seq.ofArray |> box
            else Seq.ofArray strings |> box

        | _ when t = doubleseqtype -> 

            let doubles, kos = var.ToDoubleArrayWithKnockOut()
            if removeKoPoints then filterPoints doubles kos |> Seq.ofArray |> box
            else Seq.ofArray doubles |> box

        | _ when t = intseqtype -> 

            let doubles, kos = var.ToDoubleArrayWithKnockOut()
            let ints = Array.map int doubles
            if removeKoPoints then filterPoints ints kos |> Seq.ofArray |> box
            else Seq.ofArray ints |> box

        | _ -> failwith "Unsupported type"

    /// Returns a value indicating whether a variable can be constructed from the given type.
    let CanConvertFrom (t: Type) =
        vartype.IsAssignableFrom(t) ||
        t = vartype ||
        t = inttype ||
        t = intarraytype ||
        t = intseqtype ||
        t = doubletype ||
        t = doublearraytype ||
        t = doubleseqtype ||
        t = stringtype ||
        t = stringarraytype ||
        t = stringseqtype ||
        t = booltype ||
        t = boolarraytype ||
        t = boolseqtype

    /// Converts the given object to a variable, raising an exception if the type is not supported
    /// based on the result of CanConvertFrom.
    let ConvertFrom (o:obj) =
        let t = o.GetType()
        match t with
        | _ when vartype.IsAssignableFrom(t) -> unbox<IVariable>(o)
        | _ when t = inttype         -> unbox<int>(o) 
                                        |> float 
                                        |> VariableFactory.MakeVariable

        | _ when t = doubletype      -> unbox<double>(o)    
                                        |> VariableFactory.MakeVariable

        | _ when t = stringtype      -> unbox<string>(o)    
                                        |> VariableFactory.MakeVariable

        | _ when t = booltype        -> unbox<bool>(o)      
                                        |> VariableFactory.MakeVariable

        | _ when t = intarraytype    -> unbox<int[]>(o)     
                                        |> Array.map float 
                                        |> VariableFactory.MakeVariable

        | _ when t = intseqtype      -> unbox<seq<int>>(o)  
                                        |> Seq.map float 
                                        |> Seq.toArray 
                                        |> VariableFactory.MakeVariable

        | _ when t = doublearraytype -> unbox<double[]>(o)
                                        |> VariableFactory.MakeVariable

        | _ when t = doubleseqtype   -> unbox<seq<double>>(o)
                                        |> Array.ofSeq
                                        |> VariableFactory.MakeVariable

        | _ when t = stringarraytype -> unbox<string[]>(o)  
                                        |> VariableFactory.MakeVariable

        | _ when t = stringseqtype -> unbox<seq<string>>(o)  
                                        |> Array.ofSeq
                                        |> VariableFactory.MakeVariable

        | _ when t = boolarraytype   -> unbox<bool[]>(o)    
                                        |> VariableFactory.MakeVariable

        | _ when t = boolseqtype     -> unbox<seq<bool>>(o) 
                                        |> Array.ofSeq 
                                        |> VariableFactory.MakeVariable

        | _ when t = vartype         -> unbox<IVariable>(o)

        | _ -> o.ToString() |> VariableFactory.MakeVariable

    //// Converts a variable to the generic type provided.
    /// <param name="var">The variable to convert</param>
    /// <param name="removeKoPoints">A value indicating whether knocked out points should be removed
    /// from any list like type</param>
    let ConvertTo<'a>(var: IVariable) (removeKoPoints: bool) = ConvertToType var typeof<'a> removeKoPoints |> unbox<'a>

    //// Returns a value indicating whether the variable can be converted to the generic type.
    let CanConvertTo<'a>(var: IVariable) = CanConvertToType var typeof<'a>

