namespace HullSolver

module Solver =
    open System
    open DomainTypes

    let private rnd = Random 0
    let MAX_ITERATIONS = 100
    let mutable private lastSize = 0.0
    let mutable private counter = 0

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
      List.append left right |> Seq.distinct |> List.ofSeq

    /// The main HC3 recursive algorithm.
    /// <param name="q">The "queue" (not a FIFO queue) of pairs to be processed.</param>
    /// <param name="p">All pairs.</param>
    /// <param name="vars">All variable instances.</param>
    let rec private hc3Rec (q : (Constraint * string) List) pairs vars =
        match q.Length with
        | 0 ->
            vars

        | _ ->
            let randomIdx = rnd.Next q.Length
            let pair = q.[randomIdx] // The pair will be chosen using a heuristic in the future
            let q = q |> removeAt randomIdx

            let cons = fst pair
            let variableName = snd pair
            let variable = vars
                           |> List.find (fun (item:Variable) -> item.Name = variableName)

            let reducedVariable = cons.Propagate variable vars

            match reducedVariable.Domain with
            | this when this.IsEmpty ->
                [] // The CSP is inconsistent, terminate.

            | this when variable.Domain.a = this.a && variable.Domain.b = this.b ->
                hc3Rec q pairs vars // The variable's domain has not changed - continue.

            | _ ->
               let filteredVars = reducedVariable::(vars
                                               |> List.filter (fun v -> v.Name <> reducedVariable.Name))

               let constraintsWithVar = pairs
                                        |> List.filter(fun (c, v) -> v = variable.Name)
                                        |> List.map fst

               let filteredConstraints = pairs
                                        |> List.filter(fun (c, v) -> List.contains c constraintsWithVar)

               let unitedQueue = union q filteredConstraints
               hc3Rec unitedQueue pairs filteredVars

    /// Function which prepares data for the main HC3 algorithm.
    let private hc3 (p : Problem) =
        let collectTuple (x, items) =
            items
            |> List.map (fun y -> x, y)

        let q =
            p.Constraints
            |> List.map (fun item -> (item, item.VariableNames))
            |> List.collect collectTuple

        let reducedVariables = hc3Rec q q p.Variables

        Problem(p.Constraints, reducedVariables, p.Precision)

    /// Entry function of the solver which solves the given NCSP by performing a branch-and-prune algorithm.
    let rec solve (p : Problem) =
        printfn "Box size: %f" p.Size

        if p.Size > p.Precision && (p.Size <> lastSize || counter < MAX_ITERATIONS) then
            if p.Size <> lastSize then counter <- 0 else counter <- counter + 1

            lastSize <- p.Size

            let reducedProblem = hc3 p
            if reducedProblem.HasSolution then
                let halves = reducedProblem.Split
                fst halves |> solve
                snd halves |> solve
        else
            p.Variables
            |> List.map (fun item -> printfn "%s in [%f;%f]" item.Name item.Domain.a item.Domain.b)
            |> ignore
