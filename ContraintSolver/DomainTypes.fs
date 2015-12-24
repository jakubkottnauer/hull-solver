namespace ConstraintSolver

open System

module DomainTypes =
    /// An interval
    ///
    ///   let i = { a = -1m; b = 1m }
    ///
    /// 'a' must be less than or equal to 'b'.
    type Interval =
        {
            a : decimal
            b : decimal
        }

        member this.Middle = (this.a + this.b) / 2m

        member this.Intersect (other : Interval) =
            let interB = Math.Min(this.b, other.b)
            let interA = Math.Max(this.a, other.a)

            match interA <= interB with
            | true -> { a = interA; b = interB}
            | false -> Interval.Empty

        member this.Length = if this.IsEmpty then 0m elif abs(this.b - this.a) > 0m then abs(this.b - this.a) else 1m

        member this.IsEmpty = this.a > this.b

        /// Generic binary operation over two intervals compliant with pivotal rule of interval mathematics -
        /// operation should result in the widest possible interval.
        static member operation f (x : Interval, y : Interval) =
            let list = [f x.a y.a; f x.a y.b; f x.b y.a; f x.b y.b]
            let min (xs : decimal seq) = Seq.fold (fun (acc : decimal) x -> Math.Min(acc,x)) Decimal.MaxValue xs
            let max (xs : decimal seq) = Seq.fold (fun (acc : decimal) x -> Math.Max(acc,x)) Decimal.MinValue xs
            { a =  min list; b = max list}

        static member zeroLength (x : decimal) = { a = x; b = x}

        static member Zero =  Interval.zeroLength 0m

        static member Empty = {a = 1m; b = -1m}

        static member (/) (x : Interval, y : Interval) =
            if y.a < 0m && y.b > 0m then failwith "Divisor must not be zero."
            Interval.operation (fun x y-> x/y) (x,y)

        static member (+) (x : Interval, y : Interval) = { a = x.a + y.a; b = x.b + y.b}

        static member (-) (x : Interval, y : Interval) = { a = x.a - y.b; b = x.b - y.a}

        static member (*) (x : Interval, y : Interval) = Interval.operation (fun x y -> x*y) (x,y)

        static member (*) (x : Interval, y : decimal) = x * Interval.zeroLength(y)

        static member (/) (x : Interval, y : decimal) = x / Interval.zeroLength(y)

        static member (+) (x : Interval, y : decimal) = x + Interval.zeroLength(y)

        static member (-) (x : Interval, y : decimal) = x - Interval.zeroLength(y)

        static member (*) (x : decimal, y : Interval) = Interval.zeroLength(x) * y

        static member (/) (x : decimal, y : Interval) = Interval.zeroLength(x) / y

        static member (+) (x : decimal, y : Interval) = Interval.zeroLength(x) + y

        static member (-) (x : decimal, y : Interval) = Interval.zeroLength(x) - y

        static member pow (x : Interval, p : double) = { a =  double x.a ** p |> decimal; b = double x.b ** p |> decimal }

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

        let mutable X = x
        let mutable Y = y
        let mutable Z = z

        override this.Propagate (var : Variable) (allVars : Variable list) =
            let varX = allVars |> List.find (fun (item:Variable) -> item.Name = x)
            let varY = allVars |> List.find (fun (item:Variable) -> item.Name = y)
            let varZ = allVars |> List.find (fun (item:Variable) -> item.Name = z)

            if var.Name = X then
                let ZminusY = varZ.Domain - varY.Domain
                let domain = ZminusY.Intersect varX.Domain
                Variable(var.Name, domain)

            elif var.Name = Y then
                let ZminusX = varZ.Domain - varX.Domain
                let domain = ZminusX.Intersect varY.Domain
                Variable(var.Name, domain)

            elif var.Name = Z then
                let XplusY = varX.Domain + varY.Domain
                let domain = XplusY.Intersect varZ.Domain
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
            |> Array.fold (fun acc elem -> acc * elem.Domain.Length) 1m

        /// Splits the problem into two halves by halving the first variable's domain.
        member this.Halve =
            let head = this.Variables.Head

            let half1 = Variable(head.Name, {a = head.Domain.a; b = head.Domain.Middle}) :: this.Variables.Tail
            let half2 = Variable(head.Name, {a = head.Domain.Middle; b = head.Domain.b}) :: this.Variables.Tail
            (Problem(this.Constraints, half1), Problem(this.Constraints, half2))