namespace ConstraintSolver

module Main =
    open System
    open System.Text.RegularExpressions

    let rec processInput acc =
        let input = Regex.Replace(Console.ReadLine(), @"\s+", "") // Remove all spaces from input

        if input.ToLowerInvariant() = "end" then
            acc
        else
            let tokens = input.Split('+')
            let tokens2 = tokens.[1].Split('=')

            let c = Constraint.VarPlusVarEqVarConstraint(tokens.[0], tokens2.[0], tokens2.[1]) :> Constraint.T
            processInput(c::acc)

    [<EntryPoint>]
    let main args =

        printfn "Enter a list of primitive constraints, separating them by pressing ENTER. Type 'end' when done."

        let constraints = processInput []

        let v1 = Variable("x", {a = 0m; b = 2m})
        let v2 = Variable("y", {a = 1m; b = 3m})
        let v3 = Variable("z", {a = 4m; b = 6m})
        let v4 = Variable("a", {a = -50m; b = 50m})
        let v5 = Variable("b", {a = -5m; b = -5m})
        let v6 = Variable("c", {a = 0m; b = 10m})

        let variables = [v1; v2; v3; v4; v5; v6]

        Solver.hc3 constraints variables
            |> List.map (fun item -> printfn "%s [%f;%f]" item.Name item.Domain.a item.Domain.b)
            |> ignore

        Console.ReadKey()
            |> ignore

        0