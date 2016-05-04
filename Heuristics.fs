namespace HullSolver

open System

module Heuristics =

    open DomainTypes
    let private rnd = Random DateTime.Now.Millisecond
    let private filteredVars q vars = 
        q |> List.map(fun (c, v) -> vars |> findVar v)

    /// Heuristics for selecting constraint-variable pairs.
    type Heuristics =

        /// Selects a pseudo random pair.
        static member Random (q: (Constraint * string) list) pairs (vars: Variable list) =
            rnd.Next q.Length

        /// Selects the first pair containing a dominant variable. Selects the first pair if no such is available.
        static member DominantFirst (q: (Constraint * string) list) pairs (vars: Variable list) =
            let l = q
                    |> List.map(fun (c, v) -> vars |> findVar v)
                    |> List.filter(fun v -> v.IsDominant)

            if l.Length = 0 then
                0
            else
                q |> List.findIndex(fun (c, v) -> v = l.Head.Name)

        /// Selects the first pair containing a non-dominant variable. Selects the first pair if no such is available.
        static member NonDominantFirst (q: (Constraint * string) list) pairs (vars: Variable list) =
            let l = q
                    |> List.map(fun (c, v) -> vars |> findVar v)
                    |> List.filter(fun v -> not v.IsDominant)

            if l.Length = 0 then
                0
            else
                q |> List.findIndex(fun (c, v) -> v = l.Head.Name)

        /// Selects the pair whose variable's domain has the highest right bound.
        static member MaxRightCand (q: (Constraint * string) list) pairs (vars: Variable list) =
            let max = filteredVars q vars
                        |> List.maxBy(fun item -> item.Domain.b)

            q |> List.findIndex(fun (c, v) -> v = max.Name)

        /// Selects the pair whose variable's domain has the lowest right bound.
        static member MinRightCand (q: (Constraint * string) list) pairs (vars: Variable list) =
            let min = filteredVars q vars
                        |> List.minBy(fun item -> item.Domain.b)

            q |> List.findIndex(fun (c, v) -> v = min.Name)

        /// Selects the pair whose variable's domain is the widest.
        static member LargeIntervalFirst (q: (Constraint * string) list) pairs (vars: Variable list) =
            let max = filteredVars q vars
                        |> List.maxBy(fun item -> item.Domain.Length)

            q |> List.findIndex(fun (c, v) -> v = max.Name)

        /// Selects the pair whose variable's domain is the smallest.
        static member SmallIntervalFirst (q: (Constraint * string) list) pairs (vars: Variable list) =
            let min = filteredVars q vars
                        |> List.minBy(fun item -> item.Domain.Length)

            q |> List.findIndex(fun (c, v) -> v = min.Name)

        /// Selects the pair whose variable's domain has been shrunk the most since the start of the algoritm.
        static member ShrunkMostFirst (q: (Constraint * string) list) pairs (vars: Variable list) =
            let min = filteredVars q vars
                        |> List.minBy(fun item -> item.OriginalDomain.Length / item.Domain.Length)

            q |> List.findIndex(fun (c, v) -> v = min.Name)

        /// Selects the pair whose variable's domain has been shrunk the least since the start of the algoritm.
        static member ShrunkLeastFirst (q: (Constraint * string) list) pairs (vars: Variable list) =
            let max = filteredVars q vars
                        |> List.maxBy(fun item -> item.OriginalDomain.Length / item.Domain.Length)

            q |> List.findIndex(fun (c, v) -> v = max.Name)

        /// Selects the pair whose variable is present in the largest number of pairs.
        static member FailFirst (q: (Constraint * string) list) pairs (vars: Variable list) =
            let max = filteredVars q vars
                        |> List.maxBy(fun item -> (pairs |> List.filter(fun (c, v) -> v = item.Name) |> List.length))

            q |> List.findIndex(fun (c, v) -> v = max.Name)

        /// Selects the first pair whose constraint uses addition.
        static member PreferAdd (q: (Constraint * string) list) pairs (vars: Variable list) =
            let constraints = q
                              |> List.filter(fun (c, v) -> c :? VarPlusVarEqVarConstraint)
            if constraints.Length = 0 then 0 else q |> List.findIndex(fun (c, v) -> c :? VarPlusVarEqVarConstraint)

        /// Selects the first pair whose constraint uses multiplication.
        static member PreferMult (q: (Constraint * string) list) pairs (vars: Variable list) =
            let constraints = q
                              |> List.filter(fun (c, v) -> c :? VarTimesVarEqVarConstraint)
            if constraints.Length = 0 then 0 else q |> List.findIndex(fun (c, v) -> c :? VarTimesVarEqVarConstraint)

        /// Selects the first pair (i.e. the one that has been in the queue the longest time).
        static member Fifo (q: (Constraint * string) list) pairs (vars: Variable list) =
            0