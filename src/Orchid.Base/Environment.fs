namespace Orchid.Runtime

open Orchid.TypeSystem
open Orchid.Expressions

/// A macro signature, taking the source expression as in put, and returning a new expression 
/// as the output
type MacroExpander = Expr -> Expr

/// A registry of macro expanders
type IMacroExpanderRegistry =
    
    /// Attempts to look up a macro expander by the macro name
    abstract member TryGetExpander: name:string -> MacroExpander option

/// Represents a simple interface to accessing variables
type IScope =

    /// Attempts to get a variable from the store, returning the result as an option
    abstract member Get: key:string -> IVariable option

    /// Sets a variable in the store by its  key
    abstract member Set: key:string * variable:IVariable -> unit

    /// Deletes a variable from the store
    abstract member Delete: key:string -> unit

    /// Returns a value indicating whether a variable with the given key is present
    abstract member Exists: key:string -> bool

/// Represents a container for functions in the system
type IFunctionRepository =

    /// Gets a function based on it's name, and argument count
    abstract member Get : name:string * argCount:int -> IFunction option

    /// Gets a function based on it's name
    abstract member Get : name:string -> IFunction list

    /// Returns a value indicating whether a function is present with the given name
    /// and argument count
    abstract member Exists: name:string * argCount:int -> bool
    
    /// Returns a value indicating whether a function is present with the given name
    abstract member Exists: name:string -> bool

    /// Accessor to enable enumeration of the registered functions
    abstract member Functions: seq<IFunction> with get

/// Represents the environment in which an expression can be evaluated
type IEnvironment =
    
    /// Accessor for the macro expander registry
    abstract member MacroExpanders: IMacroExpanderRegistry with get

    /// Accessor for the functions
    abstract member Functions: IFunctionRepository with get

    /// Accessor for the variable store
    abstract member Scope: IScope with get