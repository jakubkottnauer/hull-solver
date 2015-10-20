namespace ConstraintSolver

module Solver =
    open System
    let rnd = Random 0

    /// Returns a random (constraint, variable) pair. Will be replaced with heuristics in the future.
    let randomPair (x : (Constraint.T * Variable) Set) =
        x |> Set.toSeq |> Seq.item (rnd.Next(x.Count))

    /// The main HC3 recursive algorithm.
    /// "q" represents the "queue" (not a FIFO queue) of pairs to be processed.
    /// "c" contains all of the pairs.
    let rec hc3Rec (q : (Constraint.T * Variable) Set) (c : (Constraint.T * Variable) Set) =
        match q with
        | this when this.Count = 0 ->
            c

        | _ ->
            let pair = randomPair q
            let newQ = q.Remove pair

            let cons = fst pair
            let variable = snd pair
            let reducedVariable = cons.Propagate variable

            printfn "%A" cons.Expression

            match reducedVariable.Domain with
            | this when this.a = 0m && this.b = 0m ->
                c // The CSP is inconsistent, terminate.

            | this when variable.Domain.a = this.a && variable.Domain.b = this.b ->
                hc3Rec newQ c // The variable's domain has not changed - continue.

            | _ ->
               let newC = c.Remove(pair) |> Set.add(cons, reducedVariable) // Update the set of all pairs with the reduced variable.
               let constraintsWithVar = newC |> Set.filter(fun (c, v) -> v.Name = variable.Name) |> Set.map fst
               let filteredConstraints = newC |> Set.filter(fun (c, v) -> constraintsWithVar.Contains c)
               let unitedQueue = Set.union newQ filteredConstraints
               hc3Rec unitedQueue newC

    let hc3 (q : (Constraint.T * Variable) Set) =
        hc3Rec q q