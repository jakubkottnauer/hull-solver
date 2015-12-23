﻿namespace ConstraintSolver

module Solver =
    open System
    open DomainTypes

    let private rnd = Random 0

    /// Returns a random (constraint, variable) pair. Will be replaced with heuristics in the future.
    let private randomPair (x : (Constraint * string) Set) =
        x
        |> Set.toSeq
        |> Seq.item (rnd.Next(x.Count))

    /// The main HC3 recursive algorithm.
    /// "q" represents the "queue" (not a FIFO queue) of pairs to be processed.
    /// "c" contains all of the pairs.
    let rec private hc3Rec (q : (Constraint * string) Set) (c : (Constraint * string) Set) allVars =
        match q with
        | this when this.Count = 0 ->
            allVars

        | _ ->
            let pair = randomPair q
            let q = q.Remove pair

            let cons = fst pair
            let variableName = snd pair
            let variable = allVars
                           |> List.find (fun (item:Variable) -> item.Name = variableName)

            let reducedVariable = cons.Propagate variable allVars

            printfn "%A; reducing domain of %A" cons.Expression variableName

            match reducedVariable.Domain with
            | this when this.a = 0m && this.b = 0m ->
                allVars // The CSP is inconsistent, terminate.

            | this when variable.Domain.a = this.a && variable.Domain.b = this.b ->
                hc3Rec q c allVars // The variable's domain has not changed - continue.

            | _ ->
               let allVars = reducedVariable::(allVars
                                                    |> List.filter (fun item -> item.Name <> reducedVariable.Name))

               let constraintsWithVar = c
                                        |> Set.filter(fun (c, v) -> v = variable.Name)
                                        |> Set.map fst

               let filteredConstraints = c
                                        |> Set.filter(fun (c, v) -> constraintsWithVar.Contains c)

               let unitedQueue = Set.union q filteredConstraints
               hc3Rec unitedQueue c allVars

    /// Function which prepares data for the main HC3 algorithm.
    let private hc3 (p:Problem) =
        let collectTuple (x, items) =
            items
            |> List.map (fun y -> x, y)

        let q =
            p.Constraints
            |> List.map (fun item -> (item, item.VariableNames))
            |> List.collect collectTuple
            |> Set.ofList

        hc3Rec q q p.Variables


    /// Entry function of the solver which solves the given NCSP by perfoming a branch-and-bound algorithm.
    let solve p = 
        hc3 p