namespace Orchid.TypeSystem

open System

/// Enumeration representing the internal type held by a variable
type VarTypeCode =
    | Error  = 0
    | Number = 1
    | String = 2
    | Bool   = 3
    | Mixed  = 4

/// Abstract type representation of a variable
type IVariable =

    inherit IEquatable<IVariable>
    inherit IComparable<IVariable>

    /// Returns the native type for this variable
    abstract member TypeCode: VarTypeCode with get

    /// Gets the length of the variable
    abstract member Length: int with get

    /// Returns a value indicating whether the value at the given index is excluded
    abstract member IsExcluded: index: int -> bool

    /// Returns a value indicating whether the value at the given index is included
    abstract member IsIncluded: index: int -> bool

    /// Ko state at the given index
    abstract member KoState: index: int -> int

    /// Returns a value indicating whether this variable represents an error
    abstract member IsError: bool with get

    /// Returns a value indicating whether the value at the given index is automatically excluded
    abstract member IsAutoExcluded: index: int -> bool

    /// Returns a value indicating whether this variable represents a scalar
    abstract member IsScalar: bool with get

    /// Returns a value indicating whether this variable represents a vector
    abstract member IsVector: bool with get

    /// Returns a string value for the given index
    abstract member AsStringValue: index:int -> string

    /// Returns a double value for the given index
    abstract member AsDoubleValue: index:int -> double option

    /// Returns a bool value for the given index
    abstract member AsBoolValue: index:int -> bool option

    /// Constructs a variable to represent data at the given index.
    abstract member VariableAt: index: int -> IVariable