namespace ConstraintSolver

module Main =
    open System

        [<EntryPoint>]
        let main args =
            let v1 = Variable("x", {a = 0m; b = 2m})
            let v2 = Variable("y", {a = 1m; b = 3m})

            let c1 = Constraint.VarPlusVarEqConstConstraint(v1, v2, 5m) :> Constraint.T
            let c2 = Constraint.VarPlusVarEqConstConstraint(v1, v2, 2m) :> Constraint.T

            let y = Set [ c1, v1; c1, v2; c2, v1; c2, v2 ] // Set of (constraint, variable) pairs.
            let h = Solver.hc3 y
            printfn "Done."
            Console.ReadKey() |> ignore
            0