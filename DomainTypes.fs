namespace HullSolver

open System

module DomainTypes =

    let private DIVISOR_ZERO = "Divisor must not be zero."
    let private VAR_INVALID = "Invalid variable."
    let ZERO_EPSILON = 0.00000000000000000000000000000000001

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
        member this.Union (other:Interval) =
            if this.IsEmpty then other elif other.IsEmpty then this else { a = Math.Min(this.a, other.a); b = Math.Max(this.b, other.b)}

        member this.Invert =
            let inv (x:double) =
                match x with
                | 0.0 -> Double.PositiveInfinity
                | Double.PositiveInfinity -> 0.0
                | _ -> 1.0/x

            let invA = inv(this.a)
            let invB = inv(this.b)
            if invB > invA then {a = invA; b = invB} else {a = invB; b = invA}

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

        static member Negative = { a = Double.NegativeInfinity; b = 0.0}

        static member Positive = { a = 0.0; b = Double.PositiveInfinity}

        static member Empty = {a = 1.0; b = -1.0}

        static member (/) (x : Interval, y : Interval) =
            if y.a < 0.0 && y.b > 0.0 then failwith DIVISOR_ZERO
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

        member this.EqualTo y =
            abs(this.a - y.a) < ZERO_EPSILON && abs(this.b - y.b) < ZERO_EPSILON

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

    let findVar name vars =
        vars
        |> List.find (fun (item:Variable) -> item.Name = name)

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

            //printfn "%s + %s = %s; reducing the domain of %s" varX.Name varY.Name varZ.Name var.Name

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
                raise <| new ArgumentException(VAR_INVALID)

    /// A "x * y = z" constraint.
    type VarTimesVarEqVarConstraint(x: string, y: string, z: string) =
        inherit Constraint(x + " * " + y + " = " + z, [x; y; z])

        let X = x
        let Y = y
        let Z = z

        let mul_lo (a, b) : double =
            a*b

        let mul_hi (a, b) : double =
            -((-a)*b)

        let div_lo (a, b) : double =
            a/b

        let div_hi (a, b) : double =
            -(-a)/b;

        let interval_mul4 x1 x2 y1 y2 =
            if (((abs(x1) < ZERO_EPSILON) && (abs(x2) < ZERO_EPSILON)) || ((abs(y1) < ZERO_EPSILON) && (abs(y2) < ZERO_EPSILON))) then
                {a = 0.0; b = 0.0}

            elif x1 >= 0.0 then
                if y1 >= 0.0 then
                    { a=mul_lo(x1, y1); b=mul_hi(x2,y2)}
                elif y2 <= 0.0 then
                    { a=mul_lo(x2, y1); b=mul_hi(x1,y2)}
                else
                    {a=mul_lo(x2, y1);   b=mul_hi(x2,y2)}

            elif x2 <= 0.0 then
                if y1 >= 0.0 then
                    { a=mul_lo(x1, y2); b=mul_hi(x2,y1)}
                elif y2 <= 0.0 then
                    { a=mul_lo(x2, y2); b=mul_hi(x1,y1)}
                else
                    {a=mul_lo(x1, y2);   b=mul_hi(x1,y1)}

            elif y1 > 0.0 then
                { a=mul_lo(x1, y2); b=mul_hi(x2,y2)}
            elif y2 < 0.0 then
                { a=mul_lo(x2, y1); b=mul_hi(x1,y1)}
            else
                let a1 = mul_lo(x2, y1)
                let a2 = mul_lo(x1, y2)

                let b1 = mul_hi(x1, y1)
                let b2 = mul_hi(x2, y2)

                { a= Math.Min(a1, a2); b=Math.Max(b1, b2)}

        let interval_div4 x1 x2 y1 y2 =
            if y1 < 0.0 && y2 > 0.0 then
                if x1 > 0.0 then
                    ({ a = div_hi(x1, y1);  b = div_lo(x1, y2)}, false)
                elif x2 < 0.0 then
                    ({ a = div_hi(x2,y2);  b = div_lo(x2,y1)}, false)
                else
                    ({ a = Double.NegativeInfinity;  b = Double.PositiveInfinity}, true)

            elif x1 <= 0.0 && x2 >= 0.0 && y1 <= 0.0 && y2 >= 0.0 then
                ({ a = Double.NegativeInfinity;  b = Double.PositiveInfinity}, true)

            elif x1 >= 0.0 then
                if y1 >= 0.0 then
                    ({ a = div_lo(x1, y2);  b = div_hi(x2, y1)}, true)
                else
                    ({ a = div_lo(x2, y2);  b = div_hi(x1, y1)}, true)

            elif x2 <= 0.0 then
                if y1 >= 0.0 then
                    ({ a = div_lo(x1, y1);  b = div_hi(x2, y2)}, true)
                else
                    ({ a = div_lo(x2, y1);  b = div_hi(x1, y2)}, true)

            elif y1 >= 0.0 then
                ({ a = div_lo(x1, y1);  b = div_hi(x2, y1)}, true)
            else
                ({ a = div_lo(x2, y2);  b = div_hi(x1, y2)}, true)

        override this.Propagate (var : Variable) (allVars : Variable list) =
            let varX = allVars |> findVar x
            let varY = allVars |> findVar y
            let varZ = allVars |> findVar z

            //printfn "%s * %s = %s; reducing the domain of %s" varX.Name varY.Name varZ.Name var.Name

            if var.Name = X then
                Variable(var.Name, varX.Domain)

            elif var.Name = Y then
                let almostReducedY, success = interval_div4 varZ.Domain.a varZ.Domain.b varX.Domain.a varX.Domain.b

                if not success then
                    if varY.Domain.a > almostReducedY.a && varY.Domain.b < almostReducedY.b then
                            Variable(var.Name, Interval.Empty)
                        elif varY.Domain.a > almostReducedY.a || (abs(varY.Domain.a - almostReducedY.a) < ZERO_EPSILON && abs(almostReducedY.a) < ZERO_EPSILON) then
                            let reducedY = {a = almostReducedY.b; b = Double.PositiveInfinity} <*> varY.Domain
                            Variable(var.Name, reducedY)
                        elif varY.Domain.b < almostReducedY.b || (abs(varY.Domain.b - almostReducedY.b) < ZERO_EPSILON && abs(almostReducedY.b) < ZERO_EPSILON) then
                            let reducedY = {a = Double.NegativeInfinity; b = almostReducedY.a} <*> varY.Domain
                            Variable(var.Name, reducedY)
                    else
                        Variable(var.Name, Interval.Empty)
                else
                    let reducedY = almostReducedY <*> varY.Domain
                    Variable(var.Name, reducedY)

            elif var.Name = Z then
                let reducedZ = (interval_mul4 varX.Domain.a varX.Domain.b varY.Domain.a varY.Domain.b) <*> varZ.Domain
                Variable(var.Name, reducedZ)
            else
                raise <| new ArgumentException(VAR_INVALID)

    /// An NCSP problem to be solved.
    type Problem(c: Constraint list, v: Variable list, mainVars: string [], precision: double) =
        member this.Variables = v
        member this.Constraints = c
        member this.MainVars = mainVars
        member this.Precision = precision

        // TODO: Make the comparison relative to the original domain length.
        /// Returns whether the problem a whole is small enough to be considered as a solution.
        member this.IsSmallEnough : bool =
            this.Variables
            |> List.filter(fun v -> Array.contains v.Name mainVars)
            |> List.fold(fun acc item -> acc && item.Domain.Length < this.Precision) true

        /// Splits the problem into two halves by halving the first variable's domain.
        member this.Split =
            let head = this.Variables.Head

            let half1 = Variable(head.Name, {a = head.Domain.a; b = head.Domain.Middle}) :: this.Variables.Tail
            let half2 = Variable(head.Name, {a = head.Domain.Middle; b = head.Domain.b}) :: this.Variables.Tail
            (
                Problem(this.Constraints, half1, this.MainVars, this.Precision),
                Problem(this.Constraints, half2, this.MainVars, this.Precision)
            )

        member this.HasSolution = this.Variables.Length > 0

        member this.Print =
            printfn "--------------"
            printfn "Solution:"

            this.Variables
            |> List.filter(fun v -> Array.contains v.Name mainVars)
            |> List.sortBy(fun v -> v.Name)
            |> List.map (fun item -> printfn "%s in [%f;%f]" item.Name item.Domain.a item.Domain.b)
            |> ignore