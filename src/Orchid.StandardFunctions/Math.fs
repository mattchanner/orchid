namespace Orchid.Lib.Functions

open System

open Orchid.Runtime

module Math =

    [<Function("Maths", "Abs function")>]
    let Abs(d: double) = Math.Abs(d)

    [<Function("Maths", "Acos function")>]
    let Acos(d: double) = Math.Acos(d)

    [<Function("Maths", "Asin function")>]
    let Asin(d: double) = Math.Asin(d)

    [<Function("Maths", "Atan function")>]
    let Atan(d: double) = Math.Atan(d)

    [<Function("Maths", "Atan2 function")>]
    let Atan2(x: double, y:double) = Math.Atan2(x, y)

    [<Function("Maths", "Ceiling function")>]
    let Ceil(d:double) = Math.Ceiling(d)

    [<Function("Maths", "Cos function")>]
    let Cos(d:double) = Math.Cos(d)

    [<Function("Maths", "Cosh function")>]
    let Cosh(d:double) = Math.Cosh(d)

    [<Function("Maths", "Exp function")>]
    let Exp(d:double) = Math.Exp(d)

    [<Function("Maths", "Floor function")>]
    let Floor(d:double) = Math.Floor(d)

    [<Function("Maths", "Log function")>]
    let Log(d:double) = Math.Log(d)

    [<Function("Maths", "Log function")>]
    let LogBase(d:double, newBase: double) = Math.Log(d, newBase)

    [<Function("Maths", "Log function")>]
    let Log10(d:double) = Math.Log10(d)

    [<Function("Maths", "Power function, works the same as x ^ y")>]
    let Pow(x:double, y:double) = Math.Pow(x, y)

    [<Function("Maths", "Rounds a number to a value farthest away from 0")>]
    let Round(d:double) = Math.Round(d, MidpointRounding.AwayFromZero)

    [<Function("Maths", "Rounds a number to a value farthest away from 0 to the 
    specified number of fractional digits", false, "Round")>]
    let RoundTo(d:double, digits) = Math.Round(d, digits, MidpointRounding.AwayFromZero)

    [<Function("Maths", "Sin function")>]
    let Sin(d:double) = Math.Sin(d)

    [<Function("Maths", "Sinh function")>]
    let Sinh(d:double) = Math.Sinh(d)

    [<Function("Maths", "Returns the square root of a specified number")>]
    let Sqrt(d:double) = Math.Sqrt(d)

    [<Function("Maths", "Returns the tangent of the specified angle")>]
    let Tan(d:double) = Math.Tan(d)

    [<Function("Maths", "Returns the hyperbolic tangent of the specified angle")>]
    let Tanh(d:double) = Math.Tanh(d)

    [<Function("Maths", "Calculates the integral part of a specified number")>]
    let Truncate(d:double) = Math.Truncate(d)

    [<Function("Maths", "Represents the ratio of the circumference of a circle to its diameter")>]
    let PI() = Math.PI

    [<Function("Maths", "Represents the natural logarithm base, specified by the constant e")>]
    let E() = Math.E