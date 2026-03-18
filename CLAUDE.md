# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Orchid is an F#-based expression evaluation engine (XE) that provides a runtime for parsing, compiling, and evaluating expressions. It supports variable management, a function registry, and Python integration via IronPython.

## Build Commands

```bash
# Build entire solution
dotnet build Orchid.sln

# Build specific project
dotnet build src/Orchid.Base/Orchid.Base.fsproj

# Run all tests
dotnet test Orchid.sln

# Run tests with output
dotnet test Orchid.sln --logger "console;verbosity=detailed"

# Run a specific test class
dotnet test --filter "FullyQualifiedName~EvaluatorTests"

# Run a single test
dotnet test --filter "FullyQualifiedName~EvaluatorTests.can_evaluate_simple_expression"
```

## Architecture

The system follows a **Lexer → Parser → Evaluator** pipeline:

### Core Pipeline (Orchid.Expressions namespace)
- **Lexer.fs** - Tokenizes input strings. Supports numbers, strings, identifiers, operators (`+`, `-`, `/`, `*`, `^`, `%`, `>`, `>=`, `<`, `<=`, `==`, `!=`, `||`, `&&`), let bindings.
- **Parser.fs** - Builds an AST of `Expr` types using active patterns for validation.
- **Evaluator.fs** - Walks the AST to execute expressions with type coercion and lazy boolean evaluation.

### Type System (Orchid.TypeSystem namespace)
- **IVariable.fs / Variables.fs** - Core variable abstraction supporting Number (double), String, Bool types
- **Knock-out mechanism** - Tracks excluded/missing data points via `KnockoutState`
- **VariableFactory.fs** - Creates IVariable instances from various .NET types
- **VariableConverter.fs** - Converts between IVariable and .NET types for CLR function integration

### Runtime (Orchid.Runtime namespace)
- **Environment.fs** - Execution context containing Functions, Scope, MacroExpanders
- **Scope.fs** - Variable store with Get/Set/Delete/Exists operations
- **Functions.fs** - IFunction interface and IFunctionRepository for function registry

### Function Loading
- **FunctionAttribute.fs** - `[<Function(category, comment)>]` attribute to mark calculation functions
- **ClrFunctions.fs** - Reflection-based loader that scans DLLs matching "Orchid" pattern for decorated methods
- **ScriptFunction.fs** - User-defined functions written in the expression language

### Standard Library (Orchid.StandardFunctions)
- **Math.fs** - Trigonometric, logarithmic, rounding functions
- **Arrays.fs** - Reverse, DistinctList, ItemAt, Slice
- **Statistics.fs** - Statistical analysis functions
- **Constants.fs** - Mathematical constants

## Key Patterns

### Adding New Functions
Decorate static methods with the Function attribute:
```fsharp
[<Function("Math", "Computes absolute value")>]
let Abs (x: double) = System.Math.Abs(x)
```

Optional parameters: `removeKnockedoutPoints`, `customFunctionName`

### Expression Types
Discriminated union `Expr` with cases: Num, Bool, Str, Identifier, BinaryOperator, UnaryOperator, FunctionCall, Let binding. All nodes carry `TokenLocation` for error reporting.

### Namespaces
- `Orchid` - Root
- `Orchid.Expressions` - Lexer, Parser, Evaluator, Tokens
- `Orchid.TypeSystem` - IVariable, Variables, VariableFactory
- `Orchid.Runtime` - Environment, Functions, Setup
- `Orchid.Lib.Functions` - Standard function implementations

## Project Structure

```
src/
  Orchid.Base/           # Core expression engine
  Orchid.StandardFunctions/  # Built-in function library
tests/
  Orchid.Base.Tests/     # xUnit + FsUnit tests
```

## Dependencies

- .NET 10.0, F# 10.0.101
- IronPython 3.4.2 (Python integration)
- log4net 3.2.0 (async logging)
- xUnit + FsUnit (testing)
