namespace Orchid.Lib.Functions

open System

open Orchid.Runtime
open Orchid.TypeSystem

module Information =

    [<Function("Information", "Returns the numeric type code of the given variable. Possible values are
    0: Error, 
    1: Number
    2: String
    3: Bool
    4: Mixed")>]
    let TypeCodeOf (var: IVariable) = int (var.TypeCode)

    [<Function("Information", "Returns the textural type code of the given variable. Possible values are
    Error, 
    Number
    String
    Bool
    Mixed")>]
    let TypeCodeStrOf (var: IVariable) = var.TypeCode.ToString()

    [<Function("Information", "Returns true if the given variable represents an error")>]
    let IsError (var: IVariable) = var.IsError

    [<Function("Information", "Returns true if the number is even")>]
    let IsEven (num: int32) = num % 2 = 0

    [<Function("Information", "Returns true if the number is odd")>]
    let IsOdd (num: int32) = num % 2 <> 0

    [<Function("Information", "Returns true if the variable is a boolean")>]
    let IsBoolean (var: IVariable) = var.TypeCode = VarTypeCode.Bool

    [<Function("Information", "Returns true if the variable is a number")>]
    let IsNumber (var: IVariable) = var.TypeCode = VarTypeCode.Number

    [<Function("Information", "Returns true if the variable is a string")>]
    let IsString (var: IVariable) = var.TypeCode = VarTypeCode.String

    [<Function("Information", "Returns true if the variable is an array containing mixed types")>]
    let IsMixed (var: IVariable) = var.TypeCode = VarTypeCode.Mixed

    [<Function("Information", "Returns true if the variable is an array")>]
    let IsArray (var: IVariable) = var.IsVector

    [<Function("Information", "Returns true if the variable is a scalar")>]
    let IsScalar (var: IVariable) = var.IsScalar

    [<Function("Information", "Returns true if the given variable is a scalar 
    number containing an NaN (not a number) value")>]
    let IsNaN (var: IVariable) = 
        if var.IsVector || var.TypeCode <> VarTypeCode.Number 
            then false
        else
            match var.AsDoubleValue(0) with
            | Some(d) -> Double.IsNaN(d)
            | None -> false