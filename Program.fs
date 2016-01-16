namespace HullSolver

module Main =
    open System
    open System.Text.RegularExpressions
    open DomainTypes

    let UNSUPPORTED_CONSTRAINT = "Unsupported constraint format."
    let FILE_NOT_EXISTS = "File does not exist. Please specify a file containing the problem you want to solve."
    let FILE_PROMPT = "Please specify a file containing the problem you want to solve."

    let private parseConstraint text =
        let text = Regex.Replace(text, @"\s+", "")

        let tokensPlus = text.Split('+')
        let tokensMult = text.Split('*')

        if tokensPlus.Length > 1 then
            let tokens2 = tokensPlus.[1].Split('=')
            VarPlusVarEqVarConstraint(tokensPlus.[0], tokens2.[0], tokens2.[1]) :> Constraint
        elif tokensMult.Length > 1 then
            let tokens2 = tokensMult.[1].Split('=')
            VarTimesVarEqVarConstraint(tokensMult.[0], tokens2.[0], tokens2.[1]) :> Constraint
        else failwith UNSUPPORTED_CONSTRAINT

    let private parseDomain text =
        let text = Regex.Replace(text, @"\s+", "")

        let tokens = text.Split([|"in"|], StringSplitOptions.None)
        let tokens2 = tokens.[1].Split([|"["; "]"|], StringSplitOptions.None)
        let tokens3 = tokens2.[1].Split(',')
        Variable(tokens.[0], { a = double tokens3.[0]; b = double tokens3.[1]})

    let rec private validateFile path =
        let exists = System.IO.File.Exists path

        if not exists then
            printfn "%s" FILE_NOT_EXISTS
            Console.ReadLine() 
            |> validateFile

        else path

    let private parseFile path =
        let lines = System.IO.File.ReadAllLines(path)

        let constraints =
                lines
                |> Array.filter(fun line -> line.Contains("="))
                |> Array.map(fun line -> parseConstraint line)
                |> List.ofArray

        let variables =
                lines
                |> Array.filter(fun line -> line.Contains("in"))
                |> Array.map(fun line -> parseDomain line)
                |> List.ofArray

        Problem(constraints, variables)
        |> Solver.solve
        |> ignore

    [<EntryPoint>]
    let main args =
        match args with
        | [|filePath|] ->
            filePath 
            |> validateFile
            |> parseFile

        | _ ->
            printfn "%s" FILE_PROMPT
            Console.ReadLine()
            |> validateFile
            |> parseFile

        Console.ReadKey()
        |> ignore

        0