namespace ConstraintSolver

module Main =
    open System

        [<EntryPoint>]
        let main args =
            let v1 = Variable("x", {a = 0m; b = 2m})
            let v2 = Variable("y", {a = 1m; b = 3m})
            let variables = [v1; v2]

            let c1 = Constraint.VarPlusVarEqConstConstraint("x", "y", 5m) :> Constraint.T

            let constraintVariablePairs = Set [ c1, "x"; c1, "y"; ] // Set of (constraint, variable name) pairs.

            Solver.hc3 constraintVariablePairs variables
            |> List.map (fun item -> printfn "%s [%f;%f]" item.Name item.Domain.a item.Domain.b) 
            |> ignore

            Console.ReadKey() |> ignore
            0