namespace HullSolver

open System

module DomainTypes =

    /// An interval
    ///
    ///   let i = { a = -1m; b = 1m }
    ///
    /// 'a' must be less than or equal to 'b'.
    type Interval =
        {
            a : double
            b : double
        }

        member this.Middle = (this.a + this.b) / 2.0

        member this.Intersect other =
            let interB = Math.Min(this.b, other.b)
            let interA = Math.Max(this.a, other.a)

            match interA <= interB with
            | true -> { a = interA; b = interB}
            | false -> Interval.Empty

        // Presuming that the two intervals have non-zero intersection.
        member this.Union other =
            { a = Math.Min(this.a, other.a); b = Math.Max(this.b, other.b)}

        member this.Invert =
            let inv (x:double) =
                match x with
                | 0.0 -> Double.PositiveInfinity
                | Double.PositiveInfinity -> 0.0
                | _ -> 1.0/x

            {a = inv(this.a); b = inv(this.b)}

        member this.Length = if this.IsEmpty then 0.0 elif abs(this.b - this.a) > 0.0 then abs(this.b - this.a) else 1.0

        member this.IsEmpty = this.a > this.b

        /// Generic binary operation over two intervals compliant with pivotal rule of interval mathematics -
        /// operation should result in the widest possible interval.
        static member operation f (x : Interval, y : Interval) =
            let list = [f x.a y.a; f x.a y.b; f x.b y.a; f x.b y.b]
            let min (xs : double seq) = Seq.fold (fun (acc : double) x -> Math.Min(acc,x)) Double.MaxValue xs
            let max (xs : double seq) = Seq.fold (fun (acc : double) x -> Math.Max(acc,x)) Double.MinValue xs
            { a =  min list; b = max list}

        static member zeroLength (x : double) = { a = x; b = x}

        static member Zero =  Interval.zeroLength 0.0

        static member Empty = {a = 1.0; b = -1.0}

        static member (/) (x : Interval, y : Interval) =
            if y.a < 0.0 && y.b > 0.0 then failwith "Divisor must not be zero."
            Interval.operation (fun x y-> x/y) (x,y)

        static member (+) (x : Interval, y : Interval) = { a = x.a + y.a; b = x.b + y.b}

        static member (-) (x : Interval, y : Interval) = { a = x.a - y.b; b = x.b - y.a}

        static member (*) (x : Interval, y : Interval) = Interval.operation (fun x y -> x*y) (x,y)

        static member (*) (x : Interval, y : double) = x * Interval.zeroLength(y)

        static member (/) (x : Interval, y : double) = x / Interval.zeroLength(y)

        static member (+) (x : Interval, y : double) = x + Interval.zeroLength(y)

        static member (-) (x : Interval, y : double) = x - Interval.zeroLength(y)

        static member (*) (x : double, y : Interval) = Interval.zeroLength(x) * y

        static member (/) (x : double, y : Interval) = Interval.zeroLength(x) / y

        static member (+) (x : double, y : Interval) = Interval.zeroLength(x) + y

        static member (-) (x : double, y : Interval) = Interval.zeroLength(x) - y

        static member (~-) (x : Interval) = { a = -x.b; b = -x.a}

        static member (<*>) (x : Interval, y : Interval) = x.Intersect y

        static member (<+>) (x : Interval, y : Interval) = x.Union y

    /// A variable.
    type Variable(name:string, domain:Interval) =
        member this.Name = name
        member this.Domain = domain

        override this.Equals(y) =
            match y with
            | :? Variable as other -> (this.Name = other.Name)
            | _ -> false

        override x.GetHashCode() = hash x.Name
        interface IComparable with
          member x.CompareTo yvar =
              match yvar with
              | :? Variable as other -> compare x.Name other.Name
              | _ -> 0

    let findVar name (vars:Variable list) =
        vars |> List.find (fun (item:Variable) -> item.Name = name)
    
    /// A generic constraint.
    [<AbstractClass>]
    type Constraint(expression:string, variableNames: string list) =
        member this.Expression = expression

        member this.VariableNames = variableNames

        /// Enforces (propagates) the constraint on the variable domains.
        abstract member Propagate : Variable -> Variable list -> Variable

        override this.Equals(y) =
            match y with
            | :? Constraint as other -> (this.Expression = other.Expression)
            | _ -> false

        override x.GetHashCode() = hash x.Expression

        interface IComparable with
            member x.CompareTo ycons =
                match ycons with
                | :? Constraint as other -> compare x.Expression other.Expression
                | _ -> 0

    /// A "x + y = z" constraint.
    type VarPlusVarEqVarConstraint(x: string, y: string, z: string) =
        inherit Constraint(x + " + " + y + " = " + z, [x; y; z])

        let X = x
        let Y = y
        let Z = z

        override this.Propagate (var : Variable) (allVars : Variable list) =
            let varX = allVars |> findVar x
            let varY = allVars |> findVar y
            let varZ = allVars |> findVar z

            printfn "%s + %s = %s; reducing domain of %s" varX.Name varY.Name varZ.Name var.Name

            if var.Name = X then
                let ZminusY = varZ.Domain - varY.Domain
                let domain = ZminusY <*> varX.Domain
                Variable(var.Name, domain)

            elif var.Name = Y then
                let ZminusX = varZ.Domain - varX.Domain
                let domain = ZminusX <*> varY.Domain
                Variable(var.Name, domain)

            elif var.Name = Z then
                let XplusY = varX.Domain + varY.Domain
                let domain = XplusY <*> varZ.Domain
                Variable(var.Name, domain)
            else
                raise <| new ArgumentException("Invalid variable.")

    /// A "x * y = z" constraint.
    type VarTimesVarEqVarConstraint(x: string, y: string, z: string) =
        inherit Constraint(x + " * " + y + " = " + z, [x; y; z])

        let X = x
        let Y = y
        let Z = z

        override this.Propagate (var : Variable) (allVars : Variable list) =
            let varX = allVars |> findVar x
            let varY = allVars |> findVar y
            let varZ = allVars |> findVar z

            printfn "%s * %s = %s; reducing domain of %s" varX.Name varY.Name varZ.Name var.Name

            let zeroToInfty = {a = 0.0; b = Double.PositiveInfinity}

            if var.Name = X then
                if varZ.Domain = Interval.Zero then
                    let domain = varX.Domain <*> Interval.Zero
                    Variable(var.Name, domain)
                else
                    let domain = varX.Domain <*> zeroToInfty <*>
                                    (((varZ.Domain <*> zeroToInfty) * (varY.Domain <*> zeroToInfty).Invert) <+>
                                        ((-varZ.Domain <*> zeroToInfty) *
                                            (-varY.Domain <*> zeroToInfty).Invert))
                    Variable(var.Name, domain)

            elif var.Name = Y then
                let domain = varY.Domain <*> ((varX.Domain <*> zeroToInfty) * varZ.Domain).Invert
                Variable(var.Name, domain)

            elif var.Name = Z then
                let domain = varZ.Domain <*> ((varX.Domain <*> zeroToInfty) * varY.Domain)
                Variable(var.Name, domain)
            else
                raise <| new ArgumentException("Invalid variable.")

    /// An NCSP problem to be solved.
    type Problem(c: Constraint list, v: Variable list) =
        member this.Variables = v
        member this.Constraints = c

        /// Returns the current size of the box formed by the variables' domains of the problem.
        /// Calculated as the product of individual lengths of the domains.
        member this.Size =
            this.Variables
            |> Array.ofList
            |> Array.fold (fun acc elem -> acc * elem.Domain.Length) 1.0

        /// Splits the problem into two halves by halving the first variable's domain.
        member this.Halve =
            let head = this.Variables.Head

            let half1 = Variable(head.Name, {a = head.Domain.a; b = head.Domain.Middle}) :: this.Variables.Tail
            let half2 = Variable(head.Name, {a = head.Domain.Middle; b = head.Domain.b}) :: this.Variables.Tail
            (Problem(this.Constraints, half1), Problem(this.Constraints, half2))

        member this.HasSolution = this.Variables.Length > 0