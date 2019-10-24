namespace Orchid.Runtime

open System

/// Marker attribute used to flag a method inside of a class as an XE calculation
[<AttributeUsage(AttributeTargets.Method)>]
type FunctionAttribute(category:string, comment:string, removeKnockedoutPoints:bool, name:string) =
    inherit Attribute()

    /// Constructs a new instance of the <see ref="FunctionAttribute" />
    new (category:string, comment:string) = FunctionAttribute(category, comment, false, "")

    /// Constructs a new instance of the <see ref="FunctionAttribute" />
    new (category:string, comment:string, removeKnockedoutPoints:bool) = FunctionAttribute(category, comment, removeKnockedoutPoints, "")

    /// Constructs a new instance of the <see ref="FunctionAttribute" />
    new (category:string, comment:string, name:string) = FunctionAttribute(category, comment, false, name)

    /// Gets the category that this calculation should be grouped under
    member x.Category with get() = category

    /// Gets the user visible comment \ help text for this calculation
    member x.Comment with get() = comment

    /// Gets a value indicating whether this calculation should have any excluded points
    /// removed from it's inputs prior to execution
    member x.RemoveKnockedoutPoints = removeKnockedoutPoints

    /// Gets the XE name for this function.  If this value is not present the compiled name will be used instead
    member x.FunctionName = name

