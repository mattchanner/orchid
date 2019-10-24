namespace Orchid.Runtime

open System
open Orchid.TypeSystem

/// Function parameter information
type IParameter =

    /// Gets the name of the parameter
    abstract member Name: string with get

    /// Gets the position of the parameter
    abstract member Position: int with get

    /// Gets the type of the parameter
    abstract member Type: Type with get

/// Represents a function available to the runtime environment for evaluation
type IFunction =

    /// Gets the name of the function
    abstract member Name: string with get

    /// Gets the category for this function
    abstract member Category: string with get

    /// Gets the comment for this function
    abstract member Comment: string with get

    /// Gets a value indicating whether knocked out points should be removed from input
    /// variables prior to execution
    abstract member RemoveKnockedoutPoints: bool with get

    /// Returns a value indicating whether this function is deprecated
    abstract member IsDeprecated: bool with get

    /// Returns a message to provide information about why this function is deprecated
    abstract member DeprecatedMessage: string with get

    /// Returns the number of expected parameters that this function can receive
    abstract member ParameterCount: int with get

    /// Gets the list of parameters required by this function
    abstract member Parameters: IParameter list with get

    /// The return type of the function
    abstract member ReturnType: Type with get

    /// Invokes this function with the given arguments
    abstract member Invoke: args: IVariable list -> IVariable
