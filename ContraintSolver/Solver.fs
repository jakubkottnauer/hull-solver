namespace ConstraintSolver

module Solver =
    open System
    let rnd = Random 0

    /// Returns a random (constraint, variable) pair. Will be replaced with heuristics in the future.
    let randomPair (x : (Constraint.T * string) Set) =
        x
        |> Set.toSeq
        |> Seq.item (rnd.Next(x.Count))

    /// The main HC3 recursive algorithm.
    /// "q" represents the "queue" (not a FIFO queue) of pairs to be processed.
    /// "c" contains all of the pairs.
    let rec hc3Rec (q : (Constraint.T * string) Set) (c : (Constraint.T * string) Set) (allVars : Variable list) =
        match q with
        | this when this.Count = 0 ->
            allVars

        | _ ->
            let pair = randomPair q
            let newQ = q.Remove pair

            let cons = fst pair
            let variableName = snd pair
            let variable = allVars
                           |> List.find (fun (item:Variable) -> item.Name = variableName)

            let reducedVariable = cons.Propagate variableName allVars

            printfn "%A" cons.Expression

            match reducedVariable.Domain with
            | this when this.a = 0m && this.b = 0m ->
                allVars // The CSP is inconsistent, terminate.

            | this when variable.Domain.a = this.a && variable.Domain.b = this.b ->
                hc3Rec newQ c allVars // The variable's domain has not changed - continue.

            | _ ->
               let newAllVars = reducedVariable::(allVars |> List.filter (fun item -> item.Name <> reducedVariable.Name))

               let constraintsWithVar = c
                                        |> Set.filter(fun (c, v) -> v = variable.Name)
                                        |> Set.map fst

               let filteredConstraints = c
                                        |> Set.filter(fun (c, v) -> constraintsWithVar.Contains c)

               let unitedQueue = Set.union newQ filteredConstraints
               hc3Rec unitedQueue c newAllVars

    let hc3 (q : (Constraint.T * string) Set) (allVars : Variable list) =
        hc3Rec q q allVars