namespace HullSolver

module Solver =
    open System
    open DomainTypes

    let MAX_ITERATIONS = 1000
    let mutable private ind_halving_count = 0
    let mutable private ind_narrowing_count = 0

    /// Removes element from a list at the specified index.
    /// <param name="i">The index of the element to be removed.</param>
    /// <param name="l">The target list.</param>
    let rec removeAt i l =
        match i, l with
        | 0, x::xs -> xs
        | i, x::xs -> x::removeAt (i - 1) xs
        | i, [] -> failwith "Index out of range"

    /// Returns the union of two lists.
    let union left right =
      left @ right |> Seq.distinct |> List.ofSeq

    /// The main HC3 recursive algorithm.
    /// <param name="q">The "queue" (not a FIFO queue) of pairs to be processed.</param>
    /// <param name="p">All pairs.</param>
    /// <param name="vars">All variable instances.</param>
    let rec private hc3Rec (q : (Constraint * string) list) pairs vars options =
        match q.Length with
        | 0 ->
            vars

        | _ ->
            let idx = options.heuristic q pairs vars
            let cons, variableName = q.[idx]
            let q = q |> removeAt idx

            let variable = vars
                           |> List.find (fun (item:Variable) -> item.Name = variableName)

            let reducedVariable = cons.Propagate variable vars

            match reducedVariable.Domain with
            | this when this.IsEmpty ->
                [] // The CSP is inconsistent, terminate.

            | this when abs(variable.Domain.a - this.a) < ZERO_EPSILON && abs(variable.Domain.b - this.b) < ZERO_EPSILON ->
                hc3Rec q pairs vars options // The variable's domain has not changed - continue.

            | _ ->
               ind_narrowing_count <- ind_narrowing_count + 1
               let filteredVars = reducedVariable::(vars
                                               |> List.filter (fun v -> v.Name <> reducedVariable.Name))

               let constraintsWithVar = pairs
                                        |> List.filter(fun (c, v) -> v = variable.Name)
                                        |> List.map fst

               let filteredConstraints = pairs
                                        |> List.filter(fun (c, v) -> List.contains c constraintsWithVar)

               let unitedQueue = union q filteredConstraints
               hc3Rec unitedQueue pairs filteredVars options

    /// Function which prepares data for the main HC3 algorithm.
    let private hc3 options (p : Problem) =
        let collectTuple (x, items) =
            items
            |> List.map (fun y -> x, y)

        let q =
            p.Constraints
            |> List.map (fun item -> (item, item.VariableNames))
            |> List.collect collectTuple

        hc3Rec q q p.Variables options
        |> p.Clone p.WasSplitBy

    /// Recursively solves the NCSP passed into this function using a branch-and-prune algorithm.
    let rec private solveRec options (p : Problem) =
        if not(p.AllFraction options.eps) then

            let reducedProblem = hc3 options p
            if reducedProblem.HasSolution then
                let half1, half2 = reducedProblem.Split
                ind_halving_count <- ind_halving_count + 1
                solveRec options half1
                solveRec options half2

        else
              let reducedProblem = hc3 options p
              if reducedProblem.HasSolution then
                  printfn "Volume of solution box: %.32f" reducedProblem.Volume
                  reducedProblem.Print

    /// Entry function of the solver.
    let solve options (p : Problem) =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()

        solveRec options p

        stopWatch.Stop()

        printfn "Heuristic: %s" options.heuristicName
        printfn "Epsilon: %f" options.eps
        printfn "File: %s" options.fileName
        printfn "Number of narrowings: %i" ind_narrowing_count
        printfn "Number of solution halving: %i" ind_halving_count
        printfn "Original volume: %.32f" p.Volume
        printfn "Duration (s): %.32f" stopWatch.Elapsed.TotalSeconds

        printfn "---------"
