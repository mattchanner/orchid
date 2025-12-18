namespace Orchid.Runtime

open System.Collections.Generic

open IronPython
open IronPython.Hosting
open IronPython.Runtime

open Microsoft.Scripting
open Microsoft.Scripting.Hosting

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

    // Converts a sequence into a string array
    let convertPythonSeq (pt: seq<_>) =
        pt |> 
        Seq.map (fun x -> 
            if VariableConverter.CanConvertFrom(x.GetType()) then
                VariableConverter.ConvertFrom(x)
            else
                VariableFactory.MakeVariable(x.ToString())) 
        |> Seq.toArray 
        |> VariableFactory.MakeVariable
    
    // Converts the results of a script execution into a variable with special support
    // for python types as results
    let convertResult (res: obj) =
        let objType = res.GetType()
        if VariableConverter.CanConvertFrom(objType) then
            VariableConverter.ConvertFrom(res)
        elif objType = typeof<PythonTuple> then
            convertPythonSeq (res :?> PythonTuple)
        elif objType = typeof<IronPython.Runtime.PythonList> then
            convertPythonSeq (res :?> IronPython.Runtime.PythonList)
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

    let LoadScripts dir (env:IEnvironment) : seq<IFunction> =

        if not (Directory.exists dir) then 
            Seq.empty
        else
            let pyOptions = new Dictionary<string, obj>()
            //pyOptions.["DivisionOptions"] <- Pythondi.New;

#if DEBUG
            pyOptions.["Debug"] <- box true;
#endif

            let scriptEngine = Python.CreateEngine(pyOptions)
            let runtime = scriptEngine.Runtime
            let scope = scriptEngine.CreateScope()
            let functions = ResizeArray<IFunction>()

            scope.SetVariable("runtime", Runtime(env))
            scope.SetVariable("functions", functions)

            // Create an xe module in global scope
            runtime.Globals.SetVariable("orchid", scope)
            scriptEngine.SetSearchPaths([|dir|])
            runtime.LoadAssembly(typeof<System.String>.Assembly)
            runtime.LoadAssembly(typeof<IronPython.Modules.PythonRegex>.Assembly)
            runtime.LoadAssembly(typeof<System.Linq.Enumerable>.Assembly)
            runtime.LoadAssembly(typeof<System.Collections.Generic.List<_>>.Assembly)
            runtime.LoadAssembly(typeof<ScriptFunction>.Assembly)
            runtime.LoadAssembly(typeof<IVariable>.Assembly)
        
            dir
            |> Directory.fileFilter (fun f -> f.Extension = ".py")
            |> Seq.iter (fun f ->
                try
                    let script = scriptEngine.CreateScriptSourceFromFile(f.FullName)
                    let compiled = script.Compile()
                    let scope = scriptEngine.CreateScope()
                    compiled.Execute(scope) |> ignore
                with
                    | :? SyntaxErrorException as see ->
                        let eo = scriptEngine.GetService<ExceptionOperations>()
                        let formatted = eo.FormatException(see);
                        Logger.ErrorF(typeof<ScriptFunction>, "Caught exception compiling '{0}':", f.Name)
                        Logger.Error(typeof<ScriptFunction>, formatted)
                    | _ as e -> Logger.Error(typeof<ScriptFunction>, e))

            functions :> seq<IFunction>