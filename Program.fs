namespace HullSolver

module Main =
    open System
    open System.Text.RegularExpressions
    open DomainTypes

    let private UNSUPPORTED_CONSTRAINT = "Unsupported constraint format."
    let private FILE_NOT_EXISTS = "File does not exist. Please specify a file containing the problem you want to solve."
    let private FILE_PROMPT = "Please specify a file containing the problem you want to solve."
    let private PRECISION_PROMPT = "Please enter your desired precision."
    let private PRECISION_INVALID = "Invalid input. Please enter your desired precision."

    let private parseConstraint text =
        let text = Regex.Replace(text, @"\s+", "")

        let tokensPlus = text.Split '+'
        let tokensMinus = text.Split '-'
        let tokensMult = text.Split '*'

        if tokensPlus.Length > 1 then
            let tokens2 = tokensPlus.[1].Split '='
            VarPlusVarEqVarConstraint(tokensPlus.[0], tokens2.[0], tokens2.[1]) :> Constraint
        elif tokensMinus.Length > 1 then
            let tokens2 = tokensMinus.[1].Split '='
            VarPlusVarEqVarConstraint(tokens2.[0], tokens2.[1], tokensMinus.[0]) :> Constraint
        elif tokensMult.Length > 1 then
            let tokens2 = tokensMult.[1].Split '='
            VarTimesVarEqVarConstraint(tokensMult.[0], tokens2.[0], tokens2.[1]) :> Constraint
        else failwith UNSUPPORTED_CONSTRAINT

    let private parseDomain text =
        let text = Regex.Replace(text, @"\s+", "")

        let tokens = text.Split([|"in"|], StringSplitOptions.None)
        let tokens2 = tokens.[1].Split([|"["; "]"|], StringSplitOptions.None)
        let tokens3 = tokens2.[1].Split ','

        Variable(tokens.[0], { a = double tokens3.[0]; b = double tokens3.[1]})

    let rec private validateFile path =
        let exists = System.IO.File.Exists path

        if not exists then
            printfn "%s" FILE_NOT_EXISTS
            Console.ReadLine()
            |> validateFile

        else path

    let rec private validatePrecision input =
        let success, value = Double.TryParse input

        if success then value
        else
            printfn "%s" PRECISION_INVALID
            Console.ReadLine()
            |> validatePrecision

    let private parseFile path =
        let lines = System.IO.File.ReadAllLines path

        let mainVars = lines.[0].Split(' ')

        let constraints =
                lines
                |> Array.filter(fun line -> line.Contains "=")
                |> Array.map(fun line -> parseConstraint line)
                |> List.ofArray

        let variables =
                lines
                |> Array.filter(fun line -> line.Contains "in" )
                |> Array.map(fun line -> parseDomain line)
                |> List.ofArray

        (constraints, variables, mainVars)

    [<EntryPoint>]
    let main args =
        let constraints, variables, mainVars =
            match args with
            | [|filePath; precision|] ->
                filePath
            | [|filePath|] ->
                filePath
            | _ ->
                printfn "%s" FILE_PROMPT
                Console.ReadLine()

            |> validateFile
            |> parseFile

        let precision =
            match args with
            | [|filePath; precision|] ->
                precision
            | _ ->
                printfn "%s" PRECISION_PROMPT
                Console.ReadLine()
            |> validatePrecision

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()

        Problem(constraints, variables, mainVars, precision)
        |> Solver.solve
        |> ignore

        stopWatch.Stop()
        printfn "Duration (s): %f" stopWatch.Elapsed.TotalSeconds

        Console.ReadKey()
        |> ignore

        0