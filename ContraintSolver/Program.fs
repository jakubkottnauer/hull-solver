namespace ConstraintSolver

module Main =
    open System
    open System.Text.RegularExpressions
    open DomainTypes

    let rec processInput acc =
        let input = Regex.Replace(Console.ReadLine(), @"\s+", "") // Remove all spaces from input

        if input.ToLowerInvariant() = "end" then
            acc
        else
            let tokens = input.Split('+')
            let tokens2 = tokens.[1].Split('=')

            let c = VarPlusVarEqVarConstraint(tokens.[0], tokens2.[0], tokens2.[1]) :> Constraint
            processInput(c::acc)

    [<EntryPoint>]
    let main args =

        // printfn "Enter a list of primitive constraints, separating them by pressing ENTER. Type 'end' when done."

        // let constraints = processInput []

        let v1 = Variable("x", {a = 0m; b = 2m})
        let v2 = Variable("y", {a = 1m; b = 3m})
        let v3 = Variable("z", {a = 4m; b = 6m})

        let variables = [v1; v2; v3]

        let c1 = VarPlusVarEqVarConstraint("x", "y", "z") :> Constraint

        let constraints = [c1]
        
        Problem(constraints, variables)
        |> Solver.solve
        |> ignore

        Console.ReadKey()
        |> ignore

        0