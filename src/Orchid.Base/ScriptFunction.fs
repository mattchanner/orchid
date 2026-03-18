namespace Orchid.Runtime

open System
open System.Collections.Generic

open Python.Runtime

open Orchid
open Orchid.IO
open Orchid.Expressions
open Orchid.TypeSystem

module private ScriptFunctions =

    // map of type name to real type
    let knownTypes =
        dict([typeof<string>.Name,     typeof<string>
              typeof<int32>.Name,      typeof<int32>
              typeof<float>.Name,      typeof<float>
              typeof<bool>.Name,       typeof<bool>
              typeof<IVariable>.Name,  typeof<IVariable>
              typeof<string[]>.Name,   typeof<string[]>
              typeof<int32[]>.Name,    typeof<int32[]>
              typeof<bool[]>.Name,     typeof<bool[]>
              typeof<double[]>.Name,   typeof<double[]>])

    let makeTypeFromString typeString =
        if knownTypes.ContainsKey(typeString) then
            Some(knownTypes.[typeString])
        else
            None

/// A base class for scripts to extend
[<AbstractClass>]
type ScriptFunction(path:string,
                    name:string,
                    category:string,
                    comment:string,
                    isDeprecated:bool,
                    deprecatedMessage:string,
                    returnTypeAsString: string,
                    removeKnockedoutPoints: bool) =

    let mutable parameters: IParameter list = []

    // Create strongly typed return type from input string
    let returnType =
        match ScriptFunctions.makeTypeFromString returnTypeAsString with
        | Some(t) -> t
        | None -> failwith (sprintf "Unsupported return type: %s" returnTypeAsString)

    // Converts a Python sequence into an Orchid variable
    let convertPythonSeq (pyObj: PyObject) =
        use iter = pyObj.GetIterator()
        let results = ResizeArray<IVariable>()
        while iter.MoveNext() do
            use item = iter.Current
            let managed = item.AsManagedObject(typeof<obj>)
            if managed <> null && VariableConverter.CanConvertFrom(managed.GetType()) then
                results.Add(VariableConverter.ConvertFrom(managed))
            else
                results.Add(VariableFactory.MakeVariable(if managed <> null then managed.ToString() else ""))
        results.ToArray() |> VariableFactory.MakeVariable

    // Converts the results of a script execution into a variable with special support
    // for python types as results
    let convertResult (res: obj) =
        if res = null then
            VariableFactory.MakeError("Script function returned null")
        else
            let objType = res.GetType()
            if VariableConverter.CanConvertFrom(objType) then
                VariableConverter.ConvertFrom(res)
            elif typeof<PyObject>.IsAssignableFrom(objType) then
                let pyObj = res :?> PyObject
                // Check if it's a sequence type (list, tuple)
                use pyType = pyObj.GetPythonType()
                let typeName = pyType.ToString()
                if typeName.Contains("list") || typeName.Contains("tuple") then
                    convertPythonSeq pyObj
                else
                    // Try to convert to managed object
                    let managed = pyObj.AsManagedObject(typeof<obj>)
                    if managed <> null && VariableConverter.CanConvertFrom(managed.GetType()) then
                        VariableConverter.ConvertFrom(managed)
                    else
                        VariableFactory.MakeError(sprintf "Incompatible type returned from script function: %s" typeName)
            else
                VariableFactory.MakeError(sprintf "Incompatible type returned from script function: %s" (objType.FullName))

    let mkParam name position type' =
        { new IParameter with
            member x.Name with get() = name
            member x.Position with get() = position
            member x.Type with get() = type' }

    member x.FilePath with get() = path

    /// Enables parameters to be added to the script function after it has been constructed
    member x.AddParameter (name:string, type': string) =
        match ScriptFunctions.makeTypeFromString(type') with
        | Some(t) ->
            let p = mkParam name parameters.Length t
            parameters <- ((p :: parameters) |> List.rev)

        | None -> failwith (sprintf "Unsupported type %s" type')

    /// The method that the script must override
    abstract member Execute: args: obj[] -> obj

    /// Enables a script to call back into XE to invoke other library functions
    member x.ExecuteExternal(env: IEnvironment, functionName: string, args: obj[]) =
        let vars =
            args
            |> Array.map VariableConverter.ConvertFrom
            |> List.ofArray

        match env.Functions.Get(functionName, vars.Length) with
        | Some(f) -> f.Invoke(vars)
        | None -> failwith (sprintf "Unknown function %s" functionName)

    interface IFunction with

        member x.Invoke(args: IVariable list): IVariable =
            let convertedArgs = ClrFunctions.ConvertArgs parameters args removeKnockedoutPoints
            let result = x.Execute(convertedArgs)
            if result = null then
                VariableFactory.MakeError("Script function returned null")
            else
                convertResult result
        member x.Category = category
        member x.Comment = comment
        member x.DeprecatedMessage = deprecatedMessage
        member x.IsDeprecated = isDeprecated
        member x.Name = name
        member x.ParameterCount = parameters.Length
        member x.RemoveKnockedoutPoints = removeKnockedoutPoints
        member x.Parameters = parameters
        member x.ReturnType = returnType

/// Module used for loading functions from python scripts
module public ScriptLoader =

    type Runtime(env: IEnvironment) =
        member x.Environment = env
        member x.Evaluate(expr: string) =
            let result = Evaluator.evalStr env expr
            result

    let mutable private initialized = false
    let private initLock = obj()

    let private ensureInitialized () =
        if not initialized then
            lock initLock (fun () ->
                if not initialized then
                    try
                        Runtime.PythonDLL <- "python312.dll"
                        PythonEngine.Initialize()
                        initialized <- true
                    with e ->
                        Logger.ErrorF(typeof<ScriptFunction>, "Failed to initialize Python engine: {0}", e.Message))

    let LoadScripts dir (env:IEnvironment) : seq<IFunction> =

        if not (Directory.exists dir) then
            Seq.empty
        else
            ensureInitialized()

            if not initialized then
                Logger.Error(typeof<ScriptFunction>, "Python engine not initialized, skipping script loading")
                Seq.empty
            else
                let functions = ResizeArray<IFunction>()
                let runtimeObj = Runtime(env)

                try
                    use gil = Py.GIL()

                    // Get sys module and add script directory to path
                    use sys = Py.Import("sys")
                    let sysPath : PyObject = sys.GetAttr("path")
                    sysPath.InvokeMethod("insert", (0).ToPython(), dir.ToPython()) |> ignore

                    // Create a scope for the orchid module
                    use orchidScope = Py.CreateScope("orchid")
                    orchidScope.Set("runtime", runtimeObj) |> ignore
                    orchidScope.Set("functions", functions) |> ignore

                    // Add orchid module to sys.modules
                    let sysModules : PyObject = sys.GetAttr("modules")
                    sysModules.SetItem("orchid", orchidScope)

                    // Load each .py file
                    dir
                    |> Directory.fileFilter (fun f -> f.Extension = ".py")
                    |> Seq.iter (fun f ->
                        try
                            let code = System.IO.File.ReadAllText(f.FullName)
                            use scope = Py.CreateScope()
                            scope.Exec(code) |> ignore
                        with
                        | :? PythonException as pe ->
                            Logger.ErrorF(typeof<ScriptFunction>, "Python error in '{0}': {1}", f.Name, pe.Message)
                        | e ->
                            Logger.ErrorF(typeof<ScriptFunction>, "Error loading '{0}': {1}", f.Name, e.Message))
                with
                | :? PythonException as pe ->
                    Logger.ErrorF(typeof<ScriptFunction>, "Python initialization error: {0}", pe.Message)
                | e ->
                    Logger.ErrorF(typeof<ScriptFunction>, "Script loading error: {0}", e.Message)

                functions :> seq<IFunction>

    let Shutdown () =
        if initialized then
            lock initLock (fun () ->
                if initialized then
                    try
                        PythonEngine.Shutdown()
                        initialized <- false
                    with e ->
                        Logger.ErrorF(typeof<ScriptFunction>, "Error shutting down Python engine: {0}", e.Message))
