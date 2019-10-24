namespace Orchid.Runtime

open System.Collections.Generic

open Orchid.TypeSystem

module Scope =

    /// Creates an in memory scope
    let CreateInMemoryScope () =
        let dict = Dictionary<string, IVariable>()
        { new IScope with 
            member x.Get(key:string) = 
                let exists, value = dict.TryGetValue(key)
                if exists then Some(value) else None
            member x.Set(key:string, variable:IVariable) = dict.Add(key, variable)
            member x.Delete(key:string) = dict.Remove(key) |> ignore
            member x.Exists(key:string) = dict.ContainsKey(key) }

    /// Creates a new environment with a new local scope
    let CreateLocalScope (env: IEnvironment, localScope: IScope, parentScope: IScope) =

        assert (System.Object.ReferenceEquals(localScope, parentScope) = false)

        let delegatingScope = 
            { new IScope with    
                member x.Get(key:string) = 
                    let variable = localScope.Get(key)
                    if variable.IsSome then variable
                    else parentScope.Get(key)
                member x.Set(key:string, variable:IVariable) = localScope.Set(key, variable)
                member x.Delete(key:string) = localScope.Delete(key)
                member x.Exists(key:string) = 
                    if localScope.Exists(key) then true else parentScope.Exists(key) }
        
        // Create and return the environment instance
        { new IEnvironment with 
            member x.Functions      with get() = env.Functions
            member x.Scope          with get() = delegatingScope
            member x.MacroExpanders with get() = env.MacroExpanders }
