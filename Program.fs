namespace HullSolver

module Main =
    open System
    open System.Text.RegularExpressions
    open DomainTypes
    open Heuristics

    let private UNSUPPORTED_CONSTRAINT = "Unsupported constraint format."
    let private FILE_NOT_EXISTS = "File does not exist. Please specify a file containing the problem you want to solve."

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

    let private parseDomain text (dominantVars:string[]) =
        let text = Regex.Replace(text, @"\s+", "")

        let tokens = text.Split([|"in"|], StringSplitOptions.None)
        let tokens2 = tokens.[1].Split([|"["; "]"|], StringSplitOptions.None)
        let tokens3 = tokens2.[1].Split ','

        let varName = tokens.[0]
        let domain = { a = double tokens3.[0]; b = double tokens3.[1]}
        Variable(varName, domain, domain, dominantVars |> Array.contains varName)

    let rec private validateFile path =
        let exists = System.IO.File.Exists path

        if not exists then
            printfn "%s" FILE_NOT_EXISTS
            Console.ReadLine()
            |> validateFile

        else path

    let private parseFile path =
        let lines = System.IO.File.ReadAllLines path
                    |> Array.filter(fun line -> not(line.StartsWith "//") && not(String.IsNullOrWhiteSpace line))

        let constraints =
                lines
                |> Array.filter(fun line -> line.Contains "=")
                |> Array.map(fun line -> parseConstraint line)
                |> List.ofArray

        let variables =
                lines
                |> Array.filter(fun line -> line.Contains "in" )
                |> Array.map(fun line -> parseDomain line (lines.[0].Split(' ')))
                |> List.ofArray

        (constraints, variables)

    let rec parseCommandLineRec args optionsSoFar =
        match args with
        | [] ->
            optionsSoFar

        | "-f"::fileName::xs ->
            let newOptionsSoFar = { optionsSoFar with fileName=fileName}
            parseCommandLineRec xs newOptionsSoFar

        | "-p"::eps::xs ->
            let success, value = Double.TryParse eps
            if success then
                let newOptionsSoFar = { optionsSoFar with eps=value}
                parseCommandLineRec xs newOptionsSoFar
            else
                printfn "Invalid precision. Using the default value 1.0 instead."
                parseCommandLineRec xs optionsSoFar

        | "-h"::heuristicCode::xs ->
            match heuristicCode with
            | "rand" ->
                let newOptionsSoFar = { optionsSoFar with heuristic=Heuristics.Random
                                                          heuristicName="Random"}
                parseCommandLineRec xs newOptionsSoFar
            | "dom-first" ->
                let newOptionsSoFar = { optionsSoFar with heuristic=Heuristics.DominantFirst
                                                          heuristicName="Dominant-First"}
                parseCommandLineRec xs newOptionsSoFar
            | "max-cand" ->
                let newOptionsSoFar = { optionsSoFar with heuristic=Heuristics.MaxRightCand
                                                          heuristicName="Max-Right-Cand"}
                parseCommandLineRec xs newOptionsSoFar
            | "max-right-cand" ->
                let newOptionsSoFar = { optionsSoFar with heuristic=Heuristics.MaxRightCand
                                                          heuristicName="Max-Right-Cand"}
                parseCommandLineRec xs newOptionsSoFar
            | "min-right-cand" ->
                let newOptionsSoFar = { optionsSoFar with heuristic=Heuristics.MinRightCand
                                                          heuristicName="Min-Right-Cand"}
                parseCommandLineRec xs newOptionsSoFar
            | "large-int-first" ->
                let newOptionsSoFar = { optionsSoFar with heuristic=Heuristics.LargeIntervalFirst
                                                          heuristicName="Large-Interval-First"}
                parseCommandLineRec xs newOptionsSoFar
            | "small-int-first" ->
                let newOptionsSoFar = { optionsSoFar with heuristic=Heuristics.SmallIntervalFirst
                                                          heuristicName="Small-Interval-First"}
                parseCommandLineRec xs newOptionsSoFar
            | "shrunk-most-first" ->
                let newOptionsSoFar = { optionsSoFar with heuristic=Heuristics.ShrunkMostFirst
                                                          heuristicName="Shrunk-Most-First"}
                parseCommandLineRec xs newOptionsSoFar
            | "shrunk-least-first" ->
                let newOptionsSoFar = { optionsSoFar with heuristic=Heuristics.ShrunkLeastFirst
                                                          heuristicName="Shrunk-Least-First"}
                parseCommandLineRec xs newOptionsSoFar
            | "fail-first" ->
                let newOptionsSoFar = { optionsSoFar with heuristic=Heuristics.FailFirst
                                                          heuristicName="Fail-First"}
                parseCommandLineRec xs newOptionsSoFar
            | _ ->
                printfn "Unknown heuristic %s. Using the 'rand' heuristic instead." heuristicCode
                parseCommandLineRec xs optionsSoFar

        | x::xs ->
            printfn "Option '%s' is unrecognized" x
            parseCommandLineRec xs optionsSoFar

    let parseCommandLine args =
        let defaultOptions = {
            fileName = null;
            eps = 1.0;
            heuristic = Heuristics.Random;
            heuristicName = "Random";
            }

        parseCommandLineRec args defaultOptions

    [<EntryPoint>]
    let main args =
        let options = parseCommandLine (args |> List.ofArray)

//        let options = {
//             fileName = "C:\\git\\hull-solver\\tests\\solotarev";
//             eps = 0.0001;
//             heuristic = Heuristics.MinRightCand;
//             heuristicName = "max-right-cand";
//            }

        let constraints, variables =
            options.fileName
            |> validateFile
            |> parseFile

        Problem(constraints, variables)
        |> Solver.solve options
        |> ignore

        // Console.ReadKey()

        0