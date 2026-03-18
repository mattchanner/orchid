# Codebase Improvements

This document tracks potential improvements to the Orchid codebase, organized by priority.

## High Priority

### 1. Unsafe List Access Patterns

**Files:** `Evaluator.fs:148-149`, `Parser.fs:504-505, 721-722`

Direct use of `List.item` and `List.head` without bounds checking can cause `IndexOutOfRangeException` if the list is shorter than expected.

**Current:**
```fsharp
let inputs = [for i in 0 .. (argLen - 2) -> List.item i args]
let block = (List.item (argLen - 1) args)
```

**Solution:** Use `List.tryItem` with proper error handling, or pattern matching with list decomposition:
```fsharp
let inputs = args |> List.take (argLen - 1)
```

---

### 2. Unhandled Failwith Calls

**Files:** `Evaluator.fs:25, 258`

`failwith` throws exceptions instead of returning error `IVariable`s, breaking the error handling pattern used elsewhere in the codebase.

**Current:**
```fsharp
failwith "Invalid variable for bool"
failwith "Errors found during parsing"
```

**Solution:** Return `VariableFactory.MakeError()` instead, maintaining consistency with the rest of the error handling.

---

### 3. Missing Test Coverage

**Files:** `tests/Orchid.Base.Tests/`

Missing test cases for:
- Division by zero handling
- Deeply nested expressions (potential stack overflow)
- Empty input edge cases
- Invalid operator sequences
- Type mismatches in operators
- Overflow scenarios (very large exponents)
- Variable lookup failures
- Nested scope variable shadowing
- Filter/map with empty arrays
- Nested foreach/filter combinations

---

## Medium Priority

### 4. Parser-Evaluator Coupling

**File:** `Evaluator.fs:175-214`

The `evalRow` function hardcodes `item0`, `item1` variable binding logic that should be part of the scope abstraction, not the evaluator.

**Current:**
```fsharp
let isIdentifier (key:string) =
    if key.StartsWith("item") then
        if key.Length > 4 then
            let indexStr = key.Substring(4)
            let indexInt = System.Int32.Parse(indexStr)
            true, indexInt
        else true, 0
    else false, -1
```

**Solution:** Create a specialized scope implementation that handles item variables, keeping the evaluator generic.

---

### 5. Mutable Parser State

**File:** `Parser.fs:407-438`

The `IParseState` uses mutable references internally:
```fsharp
let (current: Ref<Token option>) = ref None
let (tokens:Ref<Token list>) = ref t
let (stack: Ref<MutableExpr list>) = ref []
```

This makes state threading complex, debugging harder, and blocks parallelization.

**Solution:** Consider using immutable state passing or a state monad.

---

### 6. Outdated IronPython Dependency

**File:** `Orchid.Base.fsproj`

IronPython 3.4.2 is from 2021 and is no longer maintained. May have .NET 10 compatibility issues.

**Options:**
- Migrate to `pythonnet` (Python.NET) which is actively maintained
- Make Python integration an optional/separate module
- Evaluate alternative scripting engines (Roslyn, Lua)

---

### 7. Missing Nullable Reference Types

**Files:** `*.fsproj`

Project targets .NET 10 but doesn't enable nullable reference types for static null-checking.

**Solution:** Add to project files:
```xml
<Nullable>enable</Nullable>
```

---

### 8. Result Type Underused

**Files:** Throughout codebase

Errors are returned as special `IVariable` with `VarTypeCode.Error`, conflating data values with error states.

**Current:**
```fsharp
let eval (env:IEnvironment) (expr:Expr) : IVariable
```

**Better:**
```fsharp
let eval (env:IEnvironment) (expr:Expr) : Result<IVariable, EvaluationError>
```

Benefits:
- Clearer semantics
- Compiler-enforced error handling
- Better composition with `Result` combinators

---

### 9. Assertion in Production Code

**File:** `Scope.fs:23`

```fsharp
assert (System.Object.ReferenceEquals(localScope, parentScope) = false)
```

Assertions are removed in Release builds. Should be explicit validation.

**Solution:**
```fsharp
if System.Object.ReferenceEquals(localScope, parentScope) then
    raise (ArgumentException("localScope and parentScope must be different instances"))
```

---

### 10. Reflection Overhead in ClrFunctions

**File:** `ClrFunctions.fs:100-124`

No caching of method parameter analysis. `ConvertArgs` is called fresh each invocation.

**Solution:** Cache parameter analysis and type conversion setup at function load time.

---

## Low Priority

### 11. Inefficient Sequence Operations

**File:** `ClrFunctions.fs:45`

**Current:**
```fsharp
assemblies
|> Seq.map (fun x -> seq { yield! x.GetExportedTypes() })
|> Seq.concat
```

**Better:**
```fsharp
assemblies |> Seq.collect (fun x -> x.GetExportedTypes())
```

---

### 12. String Building Overhead

**File:** `Expressions.fs:242-247`

Uses mutable `ref` for loop counter and calls `List.length` multiple times.

**Solution:** Use `List.iteri` or `Seq.iteri` to eliminate the ref cell and length call.

---

### 13. Missing XML Documentation

**Files:** Most public APIs

Public functions lack `///` XML documentation, making IDE autocomplete less helpful.

Priority files:
- `Parser.fs` - `parseString` function
- `Evaluator.fs` - `eval` function
- `Functions.fs` - `IFunction` interface members

---

### 14. Inconsistent Naming Conventions

**Examples:**
- `toString` vs `ToStringArray` vs `toSeq` - inconsistent camelCase/PascalCase
- `stdev` vs `stddev` - abbreviation inconsistency
- `Create*` functions are sometimes modules, sometimes static methods

---

### 15. Incomplete Error Messages

**File:** `Parser.fs:582`

Error messages vary in quality and lack context.

**Current:**
```fsharp
"Unknown token type found during expression parsing"
```

**Better:** Include line number, nearby tokens, and what was expected.

---

### 16. Generic Error Codes

**File:** `Parser.fs:452, 582`

Error code `1` is reused for different errors, making debugging harder.

**Solution:** Define error code constants:
```fsharp
module ErrorCodes =
    let UnknownToken = 1001
    let InvalidSequence = 1002
```

---

## Tracking

| # | Issue | Status | Notes |
|---|-------|--------|-------|
| 1 | Unsafe List Access | Done | Replaced List.item with List.take/List.last/List.tryItem/Array indexing |
| 2 | Unhandled Failwith | Done | Changed to return error variables instead of throwing |
| 3 | Missing Test Coverage | Partial | Added 8 edge case tests (division by zero, empty arrays, nested conditions, etc.) |
| 4 | Parser-Evaluator Coupling | Open | |
| 5 | Mutable Parser State | Done | Replaced IParseState interface with immutable ParseState record |
| 6 | Outdated IronPython | Done | Migrated to pythonnet 3.0.5, updated Python files to Python 3 syntax |
| 7 | Missing Nullable Types | Open | |
| 8 | Result Type Underused | Open | |
| 9 | Assertion in Production | Done | Replaced assert with invalidArg for runtime validation |
| 10 | Reflection Overhead | Open | |
| 11 | Inefficient Seq Operations | Open | |
| 12 | String Building Overhead | Open | |
| 13 | Missing XML Docs | Open | |
| 14 | Inconsistent Naming | Open | |
| 15 | Incomplete Error Messages | Open | |
| 16 | Generic Error Codes | Open | |
