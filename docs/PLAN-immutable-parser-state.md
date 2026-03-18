# Plan: Refactor Parser to Use Immutable State

## Overview

The current parser implementation in `Parser.fs` uses mutable state via `Ref<>` cells and the `IParseState` interface. This makes the code harder to reason about, debug, and potentially parallelize. This document outlines a plan to refactor the parser to use immutable state passing.

## Current Implementation Analysis

### Mutable State in IParseState (lines 321-358)

The `IParseState` interface exposes mutable operations:

```fsharp
type private IParseState =
    abstract member AddError: ParseError -> unit
    abstract member Advance: unit -> bool
    abstract member Current: Token option with get
    abstract member Tokens: Token list with get
    abstract member Stack: MutableExpr list with get
    abstract member PopExpr: unit -> MutableExpr option
    abstract member PushExpr: MutableExpr -> unit
    abstract member PeekExpr: unit -> MutableExpr option
    abstract member PeekToken: unit -> Token option
    abstract member PreviousTokens: unit -> Token list with get
```

### Implementation uses Ref cells (lines 407-438)

```fsharp
let private createParseState (t: Token list) =
    let (current: Ref<Token option>) = ref None
    let (tokens: Ref<Token list>) = ref t
    let (stack: Ref<MutableExpr list>) = ref []
    let (prior: Ref<Token list>) = ref []
    let (errors: Ref<ParseError list>) = ref []
```

### Additional Mutable Variables

- `parseGroup` (line 588): `let mutable id = ps.Current`
- `parseExpressionList` (line 618): `let mutable (acc:MutableExpr list) = []`
- `MutableExpr.BinaryOperator` uses `Ref<MutableExpr>` for left and right operands

## Proposed Solution

### Phase 1: Define Immutable ParseState Record

Replace the `IParseState` interface with an immutable record:

```fsharp
type ParseState = {
    Current: Token option
    Tokens: Token list
    Stack: MutableExpr list
    PreviousTokens: Token list
    Errors: ParseError list
}

module ParseState =
    /// Create initial parse state from token list
    let create (tokens: Token list) : ParseState =
        { Current = None
          Tokens = tokens
          Stack = []
          PreviousTokens = []
          Errors = [] }

    /// Advance to the next token
    let advance (state: ParseState) : ParseState * bool =
        match state.Tokens with
        | token :: rest ->
            { state with
                Current = Some token
                Tokens = rest
                PreviousTokens = token :: state.PreviousTokens }, true
        | [] ->
            { state with Current = None }, false

    /// Push an expression onto the stack
    let pushExpr (expr: MutableExpr) (state: ParseState) : ParseState =
        { state with Stack = expr :: state.Stack }

    /// Pop an expression from the stack
    let popExpr (state: ParseState) : ParseState * MutableExpr option =
        match state.Stack with
        | expr :: rest -> { state with Stack = rest }, Some expr
        | [] -> state, None

    /// Peek at the top expression
    let peekExpr (state: ParseState) : MutableExpr option =
        List.tryHead state.Stack

    /// Peek at the next token
    let peekToken (state: ParseState) : Token option =
        List.tryHead state.Tokens

    /// Add an error
    let addError (error: ParseError) (state: ParseState) : ParseState =
        { state with Errors = error :: state.Errors }
```

### Phase 2: Update Parser Function Signatures

Change parser functions from mutating state to returning new state:

**Current pattern:**
```fsharp
let rec private parseExpr (ps: IParseState) =
    // mutates ps
    true
```

**New pattern:**
```fsharp
let rec private parseExpr (state: ParseState) : ParseState * bool =
    // returns updated state
    newState, success
```

### Phase 3: Refactor Individual Parser Functions

#### 3.1 Helper Functions

```fsharp
/// Pushes expression and advances
let private pushExpr (expr: MutableExpr) (state: ParseState) : ParseState * bool =
    let state = ParseState.pushExpr expr state
    ParseState.advance state

/// Pushes expression without advancing
let private pushExprNoAdvance (expr: MutableExpr) (state: ParseState) : ParseState * bool =
    ParseState.pushExpr expr state, true

/// Records an error and pushes Invalid expression
let private fail (tok: Token) (code: int) (msg: string) (state: ParseState) : ParseState * bool =
    let state = ParseState.addError (ParseError(code, msg, tok)) state
    let state = ParseState.pushExpr (Invalid tok) state
    let state, _ = ParseState.advance state
    state, false
```

#### 3.2 Main parseExpr Function

```fsharp
let rec private parseExpr (state: ParseState) : ParseState * bool =
    match state.Current with
    | None -> state, false
    | Some tok ->
        match tok with
        | Token.Let(_) ->
            let state, _ = parseLetBinding state
            parseInvocation state
        | Token.SemiColon(_) ->
            let state, _ = ParseState.advance state
            state, true
        | Token.Bool(v, _) ->
            let state, _ = pushExpr (Bool(v, tok)) state
            parseInvocation state
        // ... other cases
        | _ ->
            fail tok 1 "Unknown token type found during expression parsing" state
```

#### 3.3 parseGroup Function (eliminate mutable local)

```fsharp
and private parseGroup (state: ParseState) : ParseState * bool =
    let startToken = state.Current.Value

    let rec loop state =
        match state.Current with
        | Some tok when not (Tokens.isTerminator tok) ->
            let state, _ = parseExpr state
            loop state
        | _ -> state

    let state = loop state
    let state, advanced = ParseState.advance state

    match state.Current with
    | Some tok when Tokens.isRParen tok ->
        let state, interior = ParseState.popExpr state
        let state = ParseState.pushExpr (MutableExpr.Parens(ref interior.Value, startToken)) state
        state, true
    | _ ->
        fail startToken UnmatchedRoundBracket "Unmatched round bracket" state
```

#### 3.4 parseExpressionList Function (eliminate mutable accumulator)

```fsharp
and private parseExpressionList (closingTok: Token -> bool) (state: ParseState) : ParseState * MutableExpr list =
    let state, _ = ParseState.advance state

    let rec loop state acc =
        match state.Current with
        | None -> state, List.rev acc
        | Some tok when closingTok tok -> state, List.rev acc
        | _ ->
            // Parse expressions until comma or closing token
            let rec parseUntilSeparator state =
                match state.Current with
                | Some tok when not (closingTok tok) && not (Tokens.isComma tok) ->
                    let state, _ = parseExpr state
                    parseUntilSeparator state
                | _ -> state

            let state = parseUntilSeparator state

            // Skip comma if present
            let state =
                match state.Current with
                | Some tok when Tokens.isComma tok ->
                    let state, _ = ParseState.advance state
                    state
                | _ -> state

            // Pop expression and add to accumulator
            match state.Stack with
            | [] -> loop state acc
            | _ ->
                let state, expr = ParseState.popExpr state
                loop state (expr.Value :: acc)

    let state, _ = ParseState.advance state  // Move past closing token
    state, loop state []
```

### Phase 4: Address MutableExpr Binary Operator References

The `MutableExpr.BinaryOperator` uses `Ref<MutableExpr>` to allow rewriting the right-hand side during operator precedence handling. Options:

**Option A: Keep Ref for BinaryOperator only**
- Minimal change, maintains current precedence handling
- Isolated mutation that doesn't affect parse state

**Option B: Use different AST rewriting strategy**
- Build expression tree bottom-up instead of rewriting
- More complex but fully immutable
- Would require significant changes to precedence handling

**Recommendation:** Start with Option A for pragmatic reasons, consider Option B as future enhancement.

### Phase 5: Update parseTokens and parseString

```fsharp
let private parseTokens (tokens: Token list) : ParseState =
    let rec loop state =
        let state, continue' = parseExpr state
        if continue' then loop state else state

    let initialState = ParseState.create tokens
    let state, advanced = ParseState.advance initialState
    if advanced then loop state else state

let parseString (str: string) (env: IEnvironment) =
    let tokens = Scanner.stringScanner str |> Lexer.lex
    let tokenErrors = Validator.validateTokens tokens
    if not tokenErrors.IsEmpty then
        ParseResult(List.empty, tokenErrors)
    else
        let finalState = parseTokens tokens
        let expressions = List.rev finalState.Stack |> toPublicAst
        let exprErrors = Validator.validateExpressions expressions env
        let allErrors = List.append finalState.Errors (List.ofSeq exprErrors)
        ParseResult(expressions, allErrors)
```

## Migration Strategy

### Step 1: Add Immutable ParseState alongside existing code
- Create `ParseState` record and module
- Don't modify existing functions yet
- Add unit tests for `ParseState` operations

### Step 2: Create parallel implementations
- Implement new versions of parser functions with `2` suffix (e.g., `parseExpr2`)
- Both implementations can coexist
- Validate new implementation produces same results

### Step 3: Comprehensive testing
- Create property-based tests comparing old vs new parser
- Test with edge cases: deeply nested expressions, large token lists
- Benchmark performance

### Step 4: Switch over
- Replace old implementation with new
- Remove `IParseState` interface
- Remove `createParseState` function
- Update any external code that depends on parser internals

### Step 5: Cleanup
- Remove `2` suffixes from function names
- Update documentation
- Consider removing `MutableExpr` Ref cells (Phase 4 Option B)

## Benefits

1. **Easier debugging**: State at any point can be inspected without side effects
2. **Testability**: Pure functions are easier to unit test
3. **Potential parallelization**: Immutable state enables safe concurrent parsing
4. **Clearer data flow**: Explicit state passing shows dependencies
5. **Undo/replay**: Can easily backtrack by keeping previous states

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Performance regression from record copying | F# records are efficient; benchmark to verify |
| Breaking existing behavior | Comprehensive test suite, parallel implementation |
| Increased code verbosity | Helper functions, pipeline operators |
| Binary operator precedence complexity | Keep Ref for BinaryOperator initially |

## Estimated Effort

| Phase | Effort | Dependencies |
|-------|--------|--------------|
| Phase 1: Define ParseState | 2-4 hours | None |
| Phase 2: Update signatures | 1-2 hours | Phase 1 |
| Phase 3: Refactor functions | 8-16 hours | Phase 2 |
| Phase 4: BinaryOperator refs | 4-8 hours (Option A) | Phase 3 |
| Phase 5: Integration | 2-4 hours | Phase 4 |
| Testing & validation | 4-8 hours | All phases |

**Total estimated effort: 21-42 hours**

## Success Criteria

1. All existing parser tests pass
2. No mutable `Ref<>` cells in `ParseState` (BinaryOperator exception acceptable initially)
3. No `mutable` keyword in parser functions
4. Performance within 10% of original implementation
5. Code review approval

## Future Enhancements

After successful migration, consider:

1. **Result type for errors**: Return `Result<ParseState, ParseError list>` instead of accumulating errors
2. **Computation expressions**: Create a `parser { }` computation expression for cleaner syntax
3. **Full immutability**: Remove `Ref` from `MutableExpr.BinaryOperator`
4. **Streaming parser**: Enable parsing of token streams without full list in memory
