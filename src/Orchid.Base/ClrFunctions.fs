namespace Orchid.Runtime

open System
open System.IO
open System.Reflection

open Orchid.Runtime
open Orchid.TypeSystem

/// A module representing the reflection based functions available to XE
module ClrFunctions =
    
    exception DirectoryNotFoundException of string

    // Pseudo type used for logging
    type private ClrFunctions' = { empty: int }

    /// Returns a sequence of all the dll files found within a directory
    let private filesInDir (path:string)  =
        let dirInfo = DirectoryInfo(path)
        if dirInfo.Exists = false then raise (DirectoryNotFoundException(sprintf "%s" path))
        (dirInfo.GetFiles("*.dll"))
        |> Array.toSeq

    /// Returns a sequence of loaded assemblies based off of the given sequence of files to load
    let private assemblies (files: seq<FileInfo>) =

        let safeLoad (path:string) =
            try
                Some(Assembly.LoadFrom path)
            with
                e -> Orchid.Logger.ErrorF(typeof<ClrFunctions'>, "Error loading assembly {0}: {1}", path, e)
                     None

        seq {
            for file in files do
                let loaded = safeLoad (file.FullName)
                if loaded.IsSome then yield loaded.Value
        }

    /// Returns a sequence of publicly visible types from the given sequence of assemblies
    let private exportedTypes (assemblies: seq<Assembly>)=
        assemblies
        |> Seq.map (fun x -> seq { yield! x.GetExportedTypes() } )
        |> Seq.concat

    /// Returns a sequence of methods based off of the sequence of types supplied
    let private methods (types: seq<Type>) =
        seq {
            for t in types do
                for m in t.GetMethods(BindingFlags.Public ||| 
                                      BindingFlags.Static ||| 
                                      BindingFlags.Instance) do
                    yield m
        }

    /// A type used to hold details of both a method info, and its corresponding calculation attribute
    type private FunctionInfo = { attr: FunctionAttribute; methodInfo: MethodInfo }

    /// Returns each method decorated with the calculation attribute
    let private withCalculationAttributes (methods: seq<MethodInfo>) =
        methods 
        |> Seq.choose (fun m -> 
            let attrs = m.GetCustomAttributes(typeof<FunctionAttribute>, false)
            if attrs = null || attrs.Length = 0 then None 
            else Some({attr = (attrs.[0] :?> FunctionAttribute); methodInfo = m}))
    
    /// Creates a sequence of IParameter instances to represent the arguments to the given method
    let private mkParams (m:MethodInfo) =
        seq { for p in m.GetParameters() -> 
                { new IParameter with 
                    member x.Name with get() = p.Name
                    member x.Position with get() = p.Position
                    member x.Type with get() = p.ParameterType
                } }

    let ConvertArgs (parameters: IParameter list) (args: IVariable list) (removeKoPoints:bool)=
        parameters 
        |> List.zip args
        |> List.map (fun (arg, p) -> VariableConverter.ConvertToType arg p.Type removeKoPoints)
        |> List.toArray

    /// Creates an instance of an IFunction to represent the method provided.  This factory will
    /// use the given invoker to actually invoke the method
    let private mkFunc (invoker:  IFunction -> MethodInfo -> IVariable list -> IVariable) (m:FunctionInfo) =
        let methodParams = mkParams(m.methodInfo) |> List.ofSeq
        { new IFunction with
            member x.Name = if not (String.IsNullOrEmpty(m.attr.FunctionName)) then m.attr.FunctionName else m.methodInfo.Name
            member x.Category = m.attr.Category
            member x.Comment = m.attr.Comment
            member x.IsDeprecated = false
            member x.DeprecatedMessage = ""
            member x.Parameters = methodParams
            member x.ParameterCount = m.methodInfo.GetParameters().Length
            member x.RemoveKnockedoutPoints = m.attr.RemoveKnockedoutPoints
            member x.Invoke(args:IVariable list) = invoker x m.methodInfo args
            member x.ReturnType = m.methodInfo.ReturnType }

    /// Creates an invoker to execute the method info with the strongly typed arguments
    let private invoker (func: IFunction) (m:System.Reflection.MethodInfo) (args: IVariable list) = 

        // Capture a lazy instantiation of the declaring type (used on first execution)
        let lazyInstance = lazy(Activator.CreateInstance(m.DeclaringType))

        let invokerImpl = 
            
            // Allocate an array to hold each argument
            let outputArgs = ConvertArgs func.Parameters args func.RemoveKnockedoutPoints
            
            let result = 
                try
                    if m.IsStatic then
                        m.Invoke(null, outputArgs)
                    else
                        let instance = lazyInstance.Force()
                        m.Invoke(instance, outputArgs)
                with
                    exn as e -> 
                        Orchid.Logger.ErrorF(typeof<ClrFunctions'>, "Error caught invoking function '{0}': {1}", func.Name, e)
                        VariableFactory.MakeError(e) |> box
            VariableConverter.ConvertFrom result
        
        invokerImpl

    /// Function to test whether the return type and parameter types are supported by the conversion handler
    let private isSupported (func: IFunction) = 
        let supported =
            VariableConverter.CanConvertFrom func.ReturnType && 
            (List.forall (fun (p:IParameter) -> VariableConverter.CanConvertFrom (p.Type)) func.Parameters)

        if not supported then
            Orchid.Logger.DebugF(typeof<ClrFunctions'>, "{0} is not a supported function
            (either it's return type is unsupported or one of the input arguments is not supported", func.Name)

        supported

    /// Creates and returns the functions implemented as native .NET methods, enumerated and invoked using
    /// reflection
    let LoadClrFunctionsFromPath (path:string) (env: IEnvironment) =
        path 
        |> filesInDir
        |> assemblies
        |> exportedTypes
        |> methods
        |> withCalculationAttributes
        |> Seq.map (mkFunc invoker)
        |> Seq.filter isSupported