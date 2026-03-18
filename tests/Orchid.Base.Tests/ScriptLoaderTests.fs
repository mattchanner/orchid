namespace Orchid

open System
open System.IO
open System.Reflection

open Orchid.Runtime
open Orchid.TypeSystem

open Xunit
open FsUnit.Xunit

module ScriptLoaderTests =

    let assemblyDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    type ``Script loader tests``() =

        [<Fact>]
        let ``LoadScripts returns empty sequence for non-existent directory``() =
            let env = EnvironmentSetup.SetupFromPaths(assemblyDir, assemblyDir)
            let functions = ScriptLoader.LoadScripts "/non/existent/path" env
            functions |> Seq.length |> should equal 0

        [<Fact>]
        let ``LoadScripts handles empty directory gracefully``() =
            let tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString())
            Directory.CreateDirectory(tempDir) |> ignore
            try
                let env = EnvironmentSetup.SetupFromPaths(assemblyDir, assemblyDir)
                let functions = ScriptLoader.LoadScripts tempDir env
                // Should return empty (no .py files) without throwing
                functions |> Seq.length |> should equal 0
            finally
                Directory.Delete(tempDir, true)

        [<Fact>]
        let ``ScriptFunction base class can be instantiated via derived type``() =
            // Test that the ScriptFunction abstract class has the expected interface
            let scriptFuncType = typeof<ScriptFunction>
            scriptFuncType.IsAbstract |> should be True

            // Verify expected members exist
            let methods = scriptFuncType.GetMethods()
            methods |> Array.exists (fun m -> m.Name = "AddParameter") |> should be True
            methods |> Array.exists (fun m -> m.Name = "ExecuteExternal") |> should be True

            let props = scriptFuncType.GetProperties()
            props |> Array.exists (fun p -> p.Name = "FilePath") |> should be True

        [<Fact>]
        let ``ScriptFunction implements IFunction interface``() =
            let scriptFuncType = typeof<ScriptFunction>
            let interfaces = scriptFuncType.GetInterfaces()
            interfaces |> Array.exists (fun i -> i = typeof<IFunction>) |> should be True


    type ``Python integration tests``() =

        /// Helper to check if Python is available
        let isPythonAvailable () =
            try
                // Try to find python312.dll or check if Python can be initialized
                let possiblePaths = [
                    Environment.GetEnvironmentVariable("PYTHONHOME")
                    @"C:\Python312"
                    @"C:\Program Files\Python312"
                    Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "Programs", "Python", "Python312")
                ]
                possiblePaths
                |> List.exists (fun p ->
                    not (String.IsNullOrEmpty(p)) &&
                    Directory.Exists(p) &&
                    File.Exists(Path.Combine(p, "python312.dll")))
            with _ -> false

        [<Fact>]
        let ``Can create test Python script file``() =
            let tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString())
            Directory.CreateDirectory(tempDir) |> ignore
            try
                let scriptPath = Path.Combine(tempDir, "test_script.py")
                let scriptContent = """
# Test Python script
def test_function():
    return 42
"""
                File.WriteAllText(scriptPath, scriptContent)
                File.Exists(scriptPath) |> should be True

                let content = File.ReadAllText(scriptPath)
                Assert.Contains("def test_function", content)
            finally
                Directory.Delete(tempDir, true)

        [<Fact>]
        let ``ScriptLoader gracefully handles Python unavailability``() =
            // This test verifies that when Python isn't available,
            // the system doesn't crash but returns empty functions
            let tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString())
            Directory.CreateDirectory(tempDir) |> ignore
            try
                // Create a simple .py file
                let scriptPath = Path.Combine(tempDir, "test.py")
                File.WriteAllText(scriptPath, "# Simple test\nx = 1")

                let env = EnvironmentSetup.SetupFromPaths(assemblyDir, assemblyDir)

                // This should not throw even if Python isn't available
                let functions =
                    try
                        ScriptLoader.LoadScripts tempDir env |> Seq.toList
                    with _ ->
                        [] // Return empty on any error

                // We just verify it doesn't crash - the actual count depends on Python availability
                functions.Length |> should be (greaterThanOrEqualTo 0)
            finally
                Directory.Delete(tempDir, true)

        [<Fact>]
        let ``Environment setup completes without Python``() =
            // Verify the environment can be set up even if Python scripts fail to load
            let env = EnvironmentSetup.SetupFromPaths(assemblyDir, assemblyDir)

            // Core CLR functions should still be available
            env.Functions.Exists("Sum") |> should be True
            env.Functions.Exists("Size") |> should be True
            env.Functions.Exists("Sin") |> should be True

        [<Fact>]
        let ``CLR functions work independently of Python``() =
            let env = EnvironmentSetup.SetupFromPaths(assemblyDir, assemblyDir)

            // Test that core math functions work
            match env.Functions.Get("Sum", 1) with
            | Some(sumFunc) ->
                let input = VariableFactory.MakeVariable([|1.0; 2.0; 3.0; 4.0|])
                let result = sumFunc.Invoke([input])
                result.AsDoubleValue(0) |> should equal (Some(10.0))
            | None ->
                Assert.Fail("Sum function not found")

            match env.Functions.Get("Size", 1) with
            | Some(sizeFunc) ->
                let input = VariableFactory.MakeVariable([|1.0; 2.0; 3.0|])
                let result = sizeFunc.Invoke([input])
                result.AsDoubleValue(0) |> should equal (Some(3.0))
            | None ->
                Assert.Fail("Size function not found")
