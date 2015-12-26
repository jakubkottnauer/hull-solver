﻿namespace HullSolver

module Main =
    open System
    open System.Text.RegularExpressions
    open DomainTypes

    let private parseConstraint (text:string) =
        let text = Regex.Replace(text, @"\s+", "")

        let tokens = text.Split('+')
        let tokens2 = tokens.[1].Split('=')

        VarPlusVarEqVarConstraint(tokens.[0], tokens2.[0], tokens2.[1]) :> Constraint

    let private parseDomain (text:string) =
        let text = Regex.Replace(text, @"\s+", "")

        let tokens = text.Split([|"in"|], StringSplitOptions.None)
        let tokens2 = tokens.[1].Split([|"["; "]"|], StringSplitOptions.None)
        let tokens3 = tokens2.[1].Split(',')
        Variable(tokens.[0], { a = decimal tokens3.[0]; b = decimal tokens3.[1]})

    let private parseFile (path:string) =
        let lines = System.IO.File.ReadAllLines(path)
        let constraintCount = int lines.[0]

        let constraints = lines.[1..constraintCount] |> Array.map(fun line -> parseConstraint line) |> List.ofArray
        let variables = lines.[constraintCount+1..lines.Length-1] |> Array.map(fun line -> parseDomain line) |> List.ofArray
            
        Problem(constraints, variables)
        |> Solver.solve
        |> ignore        


    [<EntryPoint>]
    let main args =
        match args with
        | [|filePath|] -> // TODO: Add input validation.
            parseFile filePath

        | _ ->
            printfn "Please specify a file containing the problem you want to solve."
            let filePath = Console.ReadLine();
            parseFile filePath

        Console.ReadKey()
        |> ignore

        0