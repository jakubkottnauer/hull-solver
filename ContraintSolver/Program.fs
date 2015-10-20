namespace ConstraintSolver

module Main =
    open System

        [<EntryPoint>]
        let main args =
            let x = Variable("x", {a = 1m; b = 5m})
            let y = Variable("y", {a = -10m; b = 5m})
            let z = Variable("z", {a = 0m; b = 1m})

            let c1 = Constraint("x-y=0")
            let c2 = Constraint("y+z=x")

            let y = Set [ c1, x; c1, y; c2, x; c2, y; c2, z ]
            let h = Solver.hc3 y

            printf "%A" h
            Console.ReadKey() |> ignore
            0