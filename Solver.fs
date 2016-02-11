﻿namespace HullSolver

module Solver =
    open System
    open DomainTypes

    let MAX_ITERATIONS = 1000
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
            let idx = options.heuristic q vars
            let cons, variableName = q.[idx]
            let q = q |> removeAt idx

            let variable = vars
                           |> List.find (fun (item:Variable) -> item.Name = variableName)

            let reducedVariable = cons.Propagate variable vars

            match reducedVariable.Domain with
            | this when this.IsEmpty ->
                [] // The CSP is inconsistent, terminate.

            | this when variable.Domain.a = this.a && variable.Domain.b = this.b ->
                hc3Rec q pairs vars options // The variable's domain has not changed - continue.

            | _ ->
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
    let private hc3 (o: Options) (p : Problem) =
        let collectTuple (x, items) =
            items
            |> List.map (fun y -> x, y)

        let q =
            p.Constraints
            |> List.map (fun item -> (item, item.VariableNames))
            |> List.collect collectTuple

        hc3Rec q q p.Variables o
        |> p.Clone p.WasSplitBy

    /// Entry function of the solver which solves the given NCSP by performing a branch-and-prune algorithm.
    let rec solve o (p:Problem) =
        //printfn "Box size: %f" p.Size

        if p.LargestSize > o.precision && counter < MAX_ITERATIONS then
            counter <- counter + 1

            let reducedProblem = hc3 o p
            if reducedProblem.HasSolution then
                let half1, half2 = reducedProblem.Split
                solve o half1
                solve o half2

        else
            let reducedProblem = hc3 o p
            if reducedProblem.HasSolution then
                reducedProblem.Print