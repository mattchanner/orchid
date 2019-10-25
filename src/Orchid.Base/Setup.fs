namespace Orchid.Runtime

open System

open Orchid

type EnvironmentSetupLogger = string

// A mutable shim to act as an environment that can have it's underlying implementation
// replaced
type private EnvironmentShim(initial: IEnvironment) =

    let mutable current = initial

    member x.Update(newEnvironment: IEnvironment) =
        current <- newEnvironment

    interface IEnvironment with
        member x.Functions = current.Functions
        member x.MacroExpanders = current.MacroExpanders
        member x.Scope = current.Scope        

module EnvironmentSetup =
    
    /// Represents a function that can return a sequence of functions based off of a given environment
    type Loader = IEnvironment -> seq<IFunction>

    /// Creates a registry of macro expanders based off of a tupled sequence
    let CreateMacroRegistry (macros: seq<string * MacroExpander>) =
        let macroDict = Map.ofSeq macros
        { new IMacroExpanderRegistry with 
                member x.TryGetExpander(name:string) = Map.tryFind name macroDict }

    /// Sets up a new environment instance using the given functions to create the necessary
    /// data that the environment encapsulates
    let Setup (scopeMaker: unit -> IScope) 
              (loaders: seq<Loader>) 
              (macroMaker: unit -> IMacroExpanderRegistry) =

        let scope = scopeMaker()
        let macroRegistry = macroMaker()

        let strCompi first second =
            String.Compare(first, second, StringComparison.InvariantCultureIgnoreCase) = 0

        let matchByName(name:string) = (fun (func:IFunction) -> strCompi (func.Name) name)

        let matchByArgs(name:string) (argCount:int) = 
            (fun (func:IFunction) -> 
                    strCompi (func.Name) name
                    && func.ParameterCount = argCount)

        let createFuncRepo (functions: IFunction list) =
            
            Logger.InfoF(typeof<EnvironmentSetupLogger>, 
                        "Function repository created with access to {0} functions", 
                        functions.Length)

            // Create the function store implementation
            { new IFunctionRepository with
                member x.Get(name:string, argCount:int) = 
                    functions |> Seq.tryFind (matchByArgs name argCount)

                member x.Get(name:string) = 
                    functions |> List.filter (matchByName name)

                member x.Exists(name:string, argCount:int) =
                    let matched = Seq.tryFind (matchByArgs name argCount) functions
                    matched.IsSome

                member x.Exists(name:string) =
                    let matched = Seq.tryFind (matchByName name) functions
                    matched.IsSome 
                
                member x.Functions with get() = List.toSeq functions }

        let createEnv (funcRepo: IFunctionRepository) =
            { new IEnvironment with
                member x.Functions = funcRepo
                member x.MacroExpanders = macroRegistry
                member x.Scope = scope }

        // Set up an initial environment with no functions
        let initialEnv = createEnv (createFuncRepo [])

        // Create a shim for this environment that can be supplied to each loader.
        // This means that any loader can maintain an object reference to the shim, 
        // which will have it's mutable environment reference updates once all functions
        // are registered
        let shim = EnvironmentShim(initialEnv)

        let functions = ResizeArray<IFunction>()
        for loader in loaders do
            Logging.logExn typeof<EnvironmentSetupLogger> (fun f ->
                let funcs = loader(shim :> IEnvironment) |> Seq.toList
                if funcs.Length > 0 then functions.AddRange(funcs))           

        // Set up the real environment with the finished list of functions
        let newEnv = createEnv (createFuncRepo (List.ofSeq functions))

        // Update the pointer that the shim has in order for loaded functions to 
        // point to the current environment instance
        shim.Update(newEnv)
        newEnv

    /// Curried function returning a Loader for clr functions
    let private createClrFunctionLoader loaderPath = ClrFunctions.LoadClrFunctionsFromPath loaderPath

    /// Curried function returning a Loader for script functions
    let private createScriptLoader loaderPath = ScriptLoader.LoadScripts loaderPath

    let SetupFromPaths (clrPath:string, scriptPath:string) =
               
        Logger.Info(typeof<EnvironmentSetupLogger>, "Setup search paths..")
        Logger.InfoF(typeof<EnvironmentSetupLogger>, "clrPath = {0}",    clrPath)
        Logger.InfoF(typeof<EnvironmentSetupLogger>, "scriptPath = {0}", scriptPath)
        
        let loaders = Seq.ofArray [|
            (createClrFunctionLoader clrPath)
            (createScriptLoader scriptPath)
        |]

        Setup (Scope.CreateInMemoryScope) loaders (fun _ -> CreateMacroRegistry Seq.empty)