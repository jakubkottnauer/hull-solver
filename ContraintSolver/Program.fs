namespace ConstraintSolver

module Main =
    open System

        [<EntryPoint>]
        let main args =
            let v1 = Variable("x", {a = 0m; b = 2m})
            let v2 = Variable("y", {a = 1m; b = 3m})
            let v3 = Variable("z", {a = 4m; b = 6m})

            let variables = [v1; v2; v3]

            let c1 = Constraint.VarPlusVarEqVarConstraint("x", "y", "z") :> Constraint.T

            let constraints = [c1]

            //let constraintVariablePairs = Set [ c1, "x"; c1, "y"; c1, "z" ] // Set of (constraint, variable name) pairs.

            Solver.hc3 constraints variables
                |> List.map (fun item -> printfn "%s [%f;%f]" item.Name item.Domain.a item.Domain.b) 
                |> ignore

            Console.ReadKey() 
                |> ignore

            0