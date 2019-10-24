namespace Orchid

open System
open System.Globalization
open System.Collections.Generic
open Microsoft.FSharp.Collections

module HashSet =

    /// Appends a value to a hashset, returning the set as the result
    let append value (s: HashSet<'T>) =
        s.Add(value) |> ignore
        s

module Seq =

    /// Returns a unique sequence of the input items with order preserved
    let unique (s: seq<'T>) =
        let resizeArr = new ResizeArray<'T>()
        let set = new HashSet<'T>()
        for t in s do
            if set.Add(t) then
                resizeArr.Add(t)
        resizeArr.ToArray()

module Array =

    /// Returns a unique array of the input items with order preserved
    let unique (arr: 'T[]) = 
        Seq.unique arr 
        |> Array.ofSeq

[<AutoOpen>]
module Extensions =
    type System.Collections.Generic.IDictionary<'Key, 'Value> with

        /// Attempts to retrieve a value from the dictionary, returning an Option<'Value>
        member x.TryGet key =
            let exists, v = x.TryGetValue(key)
            if exists then Some(v) else None

        /// Gets a value from the dictionary.  If the dictionary does not contain a value for
        // the given key, f is invoked to generate a new value to add
        member x.GetOrInit(key, (f: unit -> 'Value)): 'Value =
            match x.TryGet(key) with
            | Some(v) -> v
            | None ->
                let v = f()
                x.Add(key, v)
                v

    type System.Double with
        /// Attempts to parse the string as a double, returning the result as an Option<double>
        static member MaybeParse (s: string) =
            let ok, v = System.Double.TryParse(s)
            if ok then Some(v) else None

        /// Attempts to parse the string as a double, returning the result as an Option<double>
        static member MaybeParse (s:string, style: NumberStyles, fmt: IFormatProvider) =
            let ok, v = System.Double.TryParse(s, style, fmt)
            if ok then Some(v) else None

    type System.Int32 with
        /// Attempts to parse the string as an Int32, returning the result as an Option<Int32>
        static member MaybeParse (s: string) =
            let ok, v = System.Int32.TryParse(s)
            if ok then Some(v) else None

        /// Attempts to parse the string as an Int32, returning the result as an Option<Int32>
        static member MaybeParse (s:string, style: NumberStyles, fmt: IFormatProvider) =
            let ok, v = System.Int32.TryParse(s, style, fmt)
            if ok then Some(v) else None

    type System.Boolean with
        /// Attempts to parse the string as a Boolean, returning the result as an Option<Boolean>
        static member MaybeParse (s: string) = 
            let ok, v = System.Boolean.TryParse(s)
            if ok then Some(v) else None

    type System.String with

        /// Returns a value indicating whether this string is numeric
        member x.IsNumeric() = System.Double.MaybeParse(x).IsSome

        /// Returns a value indicating whether this string is numeric, based on an invariant culture
        member x.IsNumericInvariant() = System.Double.MaybeParse(x, NumberStyles.Any, CultureInfo.InvariantCulture).IsSome

        /// Returns a value indicating whether this string is numeric
        member x.IsNumeric(style: System.Globalization.NumberStyles, fmt: System.IFormatProvider) = 
            System.Double.MaybeParse(x, style, fmt).IsSome
        
        /// Returns an Option<Double>
        member x.MaybeDouble() = Double.MaybeParse(x)

        /// Returns an Option<Double> by parsing the string using the given number styles and format provider
        member x.MaybeDouble(style: NumberStyles, fmt: IFormatProvider) = Double.MaybeParse(x, style, fmt)

        /// Returns a double representation of the string, or Double.NaN when the string is non numeric
        member x.AsDouble() = 
            match x.MaybeDouble() with
            | Some(d) -> d
            | _ -> nan

        /// Returns a double representation of the string, or Double.NaN when the string is non numeric
        member x.AsDouble(style: NumberStyles, fmt: IFormatProvider) = 
            match x.MaybeDouble(style, fmt) with
            | Some(d) -> d
            | _ -> nan

        /// Returns an Option<Boolean>
        member x.MaybeBoolean() = Boolean.MaybeParse(x)

        /// Returns a value indicating whether the given string represents a boolean value
        member x.IsBoolean() = Boolean.MaybeParse(x).IsSome

        /// Returns an Option<Boolean>
        member x.MaybeInt32() = Int32.MaybeParse(x)

        /// Returns a value indicating whether the given string represents an integer value
        member x.IsInt32() = Int32.MaybeParse(x).IsSome

        /// Returns a value indicating whether the given string represents an integer value
        member x.IsInt32(style: NumberStyles, fmt: IFormatProvider) = Int32.MaybeParse(x, style, fmt).IsSome

        /// Returns an Option<Boolean> by parsing the string using the given number styles and format provider
        member x.MaybeInt32(style: NumberStyles, fmt: IFormatProvider) = Int32.MaybeParse(x, style, fmt)