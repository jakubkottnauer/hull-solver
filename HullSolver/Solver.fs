namespace HullSolver

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
    /// <param name="q">The "queue" (not a FIFO queue) of pairs to be processed.</param> 
    /// <param name="p">All pairs.</param>
    /// <param name="vars">All variable instances.</param>
    let rec private hc3Rec (q : (Constraint * string) Set) (p : (Constraint * string) Set) vars =
        match q.Count with
        | 0 ->
            vars

        | _ ->
            let pair = randomPair q
            let q = q.Remove pair

            let cons = fst pair
            let variableName = snd pair
            let variable = vars
                           |> List.find (fun (item:Variable) -> item.Name = variableName)

            let reducedVariable = cons.Propagate variable vars

            match reducedVariable.Domain with
            |  this when this.IsEmpty ->
                vars // The CSP is inconsistent, terminate.

            | this when variable.Domain.a = this.a && variable.Domain.b = this.b ->
                hc3Rec q p vars // The variable's domain has not changed - continue.

            | _ ->
               let vars = reducedVariable::(vars
                                               |> List.filter (fun item -> item.Name <> reducedVariable.Name))

               let constraintsWithVar = p
                                        |> Set.filter(fun (c, v) -> v = variable.Name)
                                        |> Set.map fst

               let filteredConstraints = p
                                        |> Set.filter(fun (c, v) -> constraintsWithVar.Contains c)

               let unitedQueue = Set.union q filteredConstraints
               hc3Rec unitedQueue p vars

    /// Function which prepares data for the main HC3 algorithm.
    let private hc3 (p : Problem) =
        let collectTuple (x, items) =
            items
            |> List.map (fun y -> x, y)

        let q =
            p.Constraints
            |> List.map (fun item -> (item, item.VariableNames))
            |> List.collect collectTuple
            |> Set.ofList

        let reducedVariables = hc3Rec q q p.Variables

        Problem(p.Constraints, reducedVariables)

    /// Entry function of the solver which solves the given NCSP by performing a branch-and-prune algorithm.
    let rec solve (p : Problem) =
        let epsilon = 5m

        printfn "%f" p.Size

        if p.Size > epsilon then
            let reducedProblem = hc3 p
            let halves = reducedProblem.Halve
            fst halves |> solve
            snd halves |> solve
        else
            p.Variables
            |> List.map (fun item -> printfn "%s [%f;%f]" item.Name item.Domain.a item.Domain.b)
            |> ignore