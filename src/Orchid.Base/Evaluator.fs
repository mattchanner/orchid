namespace Orchid.Expressions

open Microsoft.FSharp.Collections

open Orchid.Runtime
open Orchid.TypeSystem
open System.Collections.Generic

// Type used purely for logging
type Evaluator' = {empty: int}

/// A module to provide simple evaluation of the expression tree
module public Evaluator =
    
    // Predefined boolean values
    let True, False = VariableFactory.MakeVariable(true), VariableFactory.MakeVariable(false)

    // Ensures that all boolean values are represented by the same immutable instance
    let private makeBool value = if value then True else False

    // Conversion to bool option
    let private bool (v:IVariable) =
        match v.AsBoolValue(0) with
        | Some(b) -> b
        | _ -> failwith "Invalid variable for bool"

    /// Tests that all supplied inputs are of equal length
    let private allInputsHaveEqualLength (inputs: IVariable list) =
        
        // Map each var to its length
        let lengths = List.map (fun (x:IVariable) -> x.Length) inputs
        
        // Validate the min and max of the lengths are the same (i.e. all inputs are of equal size)
        let lower, upper = (lengths |> List.min), (lengths |> List.max)
        
        // return tuple of (all equal * size)
        (lower = upper, lower)

    /// A simple evaluation engine for expressions (overloaded to support direct evaluation of expressions)
    [<CompiledName("Eval")>]
    let rec eval (env:IEnvironment) (expr:Expr) : IVariable =
        
        match expr with

        | Expr.Assign(Expr.Identifier(name, _), expr, _) ->
            let result = eval env expr
            env.Scope.Set(name, result)
            result

        // Simple literals
        | Expr.Bool(b, _)   -> makeBool(b)
        | Expr.Str(s, _, _) -> VariableFactory.MakeVariable(s)
        | Expr.Num(dbl, _)  -> VariableFactory.MakeVariable(dbl)

        // Operators
        | Expr.UnaryOperator(op, expr, _) ->
            let right = eval env expr
            match op with
            | Op.Minus -> 
                if right.IsScalar then 
                    match right.AsDoubleValue(0) with
                    | Some(d) -> VariableFactory.MakeVariable(-(d))
                    | _ -> VariableFactory.MakeError("Only numbers permitted for unary operation")
                else
                    VariableFactory.MakeError("Only numbers permitted for unary operation")
            |_ -> VariableFactory.MakeError("Only minus operator permitted for unary operation")

        | Expr.BinaryOperator(lhs, op, rhs, _) ->
            // Make evaluation lazy in order to honour lazy evaluation of && operator
            let left = eval env lhs
            let right = eval env rhs
            match op with
            | Op.Eq      -> makeBool(left.Equals(right))
            | Op.Gt      -> makeBool(left.CompareTo(right) > 0)
            | Op.Gte     -> makeBool(left.CompareTo(right) >= 0)
            | Op.Lt      -> makeBool(left.CompareTo(right) < 0)
            | Op.Lte     -> makeBool(left.CompareTo(right) <= 0)
            | Op.NEq     -> makeBool(not (left.Equals right))
            | Op.Div     -> left.DivideBy(right)
            | Op.Minus   -> left.Subtract(right)
            | Op.Mod     -> left.Modulus(right)
            | Op.Mul     -> left.MultiplyBy(right)
            | Op.Plus    -> left.Add(right)
            | Op.Pow     -> left.Pow(right)
            | Op.And     -> left.AndWith(right)
            | Op.Or      -> left.OrWith(right)
            | _ -> VariableFactory.MakeError("Unknown operator given")

        | Expr.Parens(exp, _) -> eval env exp
        // Internal statement handlers

        | Expr.AnonymousFunc(ls, _) -> 
            ls 
                |> Seq.map (eval env) 
                |> VariableFactory.MakeVariable

        | Expr.Not(e, _) -> 
            e 
            |> (eval env) 
            |> bool 
            |> not 
            |> makeBool

        | Expr.IfThenElse(ifexpr, thenexpr, elseexpr, t) -> 
            if bool (eval env ifexpr) then eval env thenexpr else eval env elseexpr

        | Expr.Or(exprs, t) ->
            // Enumerate all expressions, exiting on first occurrence of a true result
            let rec evalrec vals =
                match vals with
                | x::xs -> if bool (eval env x) then true else evalrec xs
                | _ -> false
            makeBool(evalrec exprs)

        | Expr.And(exprs, t) ->
            // Map each expr into a lazy evaluation, then check each expr returns true.  This means
            // that the evaluation will stop on first failure, and subsequent expressions will not 
            // be evaluated
            exprs
            |> List.map (fun expr -> lazy(eval env expr))
            |> List.forall (fun lzy -> bool (lzy.Force()))
            |> makeBool 

        | Expr.Array(exprs, _) ->
            exprs 
            |> Seq.map (eval env)
            |> VariableFactory.MakeVariable

        // Variable lookup using environment
        | Expr.Variable(name, _) | Expr.Identifier(name, _) -> 
            match env.Scope.Get(name) with
            | Some(variable) -> variable
            | None -> VariableFactory.MakeError(sprintf "No variable found with name %s" name)

        // Function invocation using environment
        | Expr.Function(Expr.Identifier(name, _), args, _) ->
            match env.Functions.Get(name, (List.length args)) with
            | Some(func) ->
                let values = List.map (eval env) args
                func.Invoke(values)
            | None -> VariableFactory.MakeError(sprintf "Unable to locate function named %s" name)            
        
        | Expr.ForEach(args, _) ->

            // foreach expects at least one input and a final anonymous function
            let argLen = args.Length
            let inputs = [for i in 0 .. (argLen - 2) -> List.item i args] |> List.map (eval env)
            let block = (List.item (argLen - 1) args)

            evalForEach inputs block env

        // Filter function expects 2 arguments - first is a list, second is an expression to apply 
        // to each item
        | Expr.Filter(input, Expr.AnonymousFunc([filterBlock], _), _) ->
            let source = eval env input
            seq { for i in 0 .. (source.Length - 1) -> source.VariableAt(i) }
                |> Seq.map (fun x -> (x, evalRow filterBlock env [|x|]))
                |> Seq.filter (fun (x, var) -> bool var)
                |> Seq.map (fun (x, _) -> x)
                |> VariableFactory.MakeVariable            
        
        // Macro expression - locate an expander, expand into a new expression, then evaluate
        | Expr.Macro(expr, _) ->
            match expr with
            | Expr.Identifier(ident, _) ->
                match env.MacroExpanders.TryGetExpander(ident) with 
                | Some(expander) -> eval env (expander expr)
                | _ -> VariableFactory.MakeError(sprintf "Unknown macro found: %s" ident)
            | _ -> eval env expr
        | _ -> VariableFactory.MakeError("Unsupported type for evaluator")
    
    /// evaluates a block expression using a row of variables to be used
    /// whenever a variable request is made for a named variable item0 item 1 etc
    and private evalRow (block: Expr) (env:IEnvironment) (vars: IVariable[]) =

        // Tests whether the requested key is for an identifier, and if it is, returns
        // the index of the requested variable
        let isIdentifier (key:string) =
            if key.StartsWith("item") then
                if key.Length > 4 then
                    let indexStr = key.Substring(4)
                    let indexInt = System.Int32.Parse(indexStr)
                    true, indexInt
                else
                    true, 0
            else
                false, -1

        // A local variable store to return identifiers upon request
        let localScope = 
            let memoryScope = Dictionary<string, IVariable>()
            { new IScope with
               
                member x.Get(key:string) =
                    // if asked for an item, get it from the array if possible, otherwise 
                    // delegate to the real store
                    let isIdentifier, index = isIdentifier key
                    if not isIdentifier then None else Some(vars.[index])

                member x.Set(key:string, variable:IVariable) = memoryScope.Add(key, variable)
               
                member x.Delete(key:string) = memoryScope.Remove(key) |> ignore
               
                member x.Exists(key:string) =
                    // Check variable existence based on whether it is an identifier, and whether
                    // the index refers to an element in the source array
                    let isIdentifier, index = isIdentifier key
                    if isIdentifier then index < vars.Length 
                    elif memoryScope.ContainsKey(key) then true
                    else false }

        // Construct a new local scope for this evaluation in order for locals to be stored
        eval (Scope.CreateLocalScope(env, localScope, env.Scope)) block
        
    /// evaluates a foreach block
    and private evalForEach (inputs: IVariable list) (block: Expr) (env:IEnvironment) : IVariable =

        let numCols = inputs.Length
        if numCols = 0 then VariableFactory.MakeError("No inputs given to foreach") else
        let allEqual, numRows = allInputsHaveEqualLength inputs

        // Validate lengths match up
        if not allEqual then VariableFactory.MakeError("Lists must be of equal size") else
        
        // Create an array of variables to represent each row in the input table
        let slices = seq { for y in 0 .. (numRows - 1) do
                            yield [| 
                                for x in 0 .. (numCols - 1) -> 
                                    (List.item x inputs) 
                                        |> (fun v -> v.VariableAt(y)) |] }
        
        slices 
            |> Seq.map (evalRow block env)
            |> VariableFactory.MakeVariable

    /// Evaluates a list of expressions
    [<CompiledName("Eval")>]
    let evalAll (env:IEnvironment) (expressions: Expr list) =
        
        // Create a new environment with a top level local scope to handle local assignments
        let scopedEnv = Scope.CreateLocalScope(env, (Scope.CreateInMemoryScope()), (env.Scope))

        // Create an array of evaluation results, so that the last one can be returned 
        // (all other evaluations must be assignments to local variables)
        let vars = [|for expr in expressions -> eval scopedEnv expr|]

        vars.[vars.Length - 1]

    /// Evaluates a string expression
    [<CompiledName("Eval")>]
    let evalStr (env:IEnvironment) (expr:string) =
        
        Orchid.Logger.DebugF(typeof<Evaluator'>, "Evaluating expression: '{0}'", expr)

        let result = Parser.parseString expr env
        if not (result.Errors.IsEmpty) then
            failwith "Errors found during parsing of the input expression"
        elif result.Expressions.Length > 1 then
            let vars = [|for expr in result.Expressions -> eval env expr|]
            vars.[vars.Length - 1]
        else
            eval env (List.item 0 (result.Expressions))