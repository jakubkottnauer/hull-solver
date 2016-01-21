namespace HullSolver

open System

module DomainTypes =

    let private DIVISOR_ZERO = "Divisor must not be zero."
    let private VAR_INVALID = "Invalid variable."
    let private ZERO_EPSILON = 0.00000001

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

            printfn "%s + %s = %s; reducing the domain of %s" varX.Name varY.Name varZ.Name var.Name

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

        let rec DivBounds (i1 : Interval, i2 : Interval) =
            if i1.a <= 0.0 && i1.b >= 0.0 && i2.a <= 0.0 && i2.b >= 0.0 then
                {a = Double.NegativeInfinity; b = Double.PositiveInfinity}

//            elif i2.a == 0.0 && i2.b == 0.0 && (i1.a > 0.0 || i1.b < 0.0) then
//                throw Store.failException;

            elif i2.a < 0.0 && i2.b > 0.0 && (i1.a > 0.0 || i1.b < 0.0) then
                let max = Math.Max(abs(i1.a), abs(i1.b))
                let min = -max
                {a = min; b = max}

            elif abs(i2.a) < ZERO_EPSILON && abs(i2.b) > ZERO_EPSILON && (i1.a > 0.0 || i1.b < 0.0) then
                DivBounds({a = i1.a; b = i1.b}, {a = 1.0; b = i2.b});
            elif abs(i2.a) > ZERO_EPSILON && abs(i2.b) < ZERO_EPSILON && (i1.a > 0.0 || i1.b < 0.0) then
                DivBounds({a = i1.a; b = i1.b}, {a = i2.a; b = -1.0});

            else
                let ac = i1.a / i2.a
                let ad = i1.a / i2.b
                let bc = i1.b / i2.a
                let bd = i1.b / i2.b
                let min = Math.Min(Math.Min(ac, ad), Math.Min(bc, bd));
                let max = Math.Max(Math.Max(ac, ad), Math.Max(bc, bd));
                {a = min; b = max}

        let rec MulBounds (i1 : Interval, i2 : Interval) =
            let M_1 =  (i1.a < 0.0 && i1.b > 0.0)        // contains zero
            let Z_1 =  (abs(i1.a) < ZERO_EPSILON  && abs(i1.b) < ZERO_EPSILON)     // zero
            let P0_1 = (abs(i1.a) < ZERO_EPSILON && i1.b > 0.0)       // positive with zero
            let P1_1 = (i1.a > 0.0 && i1.b > 0.0)      // strictly positive
            let N0_1 = (i1.a < 0.0 && abs(i1.b) < ZERO_EPSILON)       // negative with zero
            let N1_1 = (i1.a < 0.0 && i1.b < 0.0)        // strictly negative

            let M_2 =  (i2.a < 0.00 && i2.b > 0.0)
            let Z_2 =  (abs(i2.a) < ZERO_EPSILON  && abs(i2.b) < ZERO_EPSILON)
            let P0_2 = (abs(i2.a) < ZERO_EPSILON && i2.b > 0.0)
            let P1_2 = (i2.a > 0.0 && i2.b > 0.0)
            let N0_2 = (i2.a < 0.0 && abs(i2.b) < ZERO_EPSILON)
            let N1_2 = (i2.a < 0.0 && i2.b < 0.0)

            if P1_1 then
                if P1_2 then // P1 /\ P1
                    let min =  floor i1.a * i2.a
                    let max = ceil i1.b*i2.b
                    {a = min; b = max}
                elif P0_2 then // P1 /\ P0
                    let min = 0.0; //down(a*c);
                    let max = ceil i1.b*i2.b
                    {a = min; b = max}
                elif M_2 then // P1 /\ M
                    let min = floor i1.b*i2.a
                    let max = ceil i1.b*i2.b
                    {a = min; b = max}
                elif N1_2 then // P1 /\ N1
                    let min = floor i1.b*i2.a
                    let max = ceil i1.a*i2.b
                    {a = min; b = max}
                elif N0_2 then // P1 /\ N0
                    let min =  floor i1.b*i2.a
                    let max = 0.0; // up(a*d);
                    {a = min; b = max}
                else // P1 /\ Z
                    {a = 0.0; b = 0.0}

            elif P0_1 then
                if P1_2 || P0_2 then // P0 /\ { P1 \/ P0}
                    let min = 0.0
                    let max = ceil i1.b*i2.b
                    {a = min; b = max}
                 elif (N1_2 || N0_2) then //P0 /\ { N0 \/ N1 }
                    let min = floor i1.b*i2.a
                    let max = 0.0; //ceil a*d);
                    {a = min; b = max}
                 elif (M_2) then // P0 /\ M
                    let min = floor i1.b*i2.a
                    let max = ceil i1.b*i2.b
                    {a = min; b = max}
                 else //if (Z_2) // P0 /\ Z
                    {a = 0.0; b = 0.0}

            elif (M_1) then
                if P0_2 || P1_2 then // M /\ { P0 \/ P1}
                    let min = floor i1.a*i2.b
                    let max = ceil i1.b*i2.b
                    {a = min; b = max}

                elif (N0_2 || N1_2 ) then // M /\ { N0 \/ N1}
                    let min = floor i1.b*i2.a
                    let max = ceil i1.a*i2.a
                    {a = min; b = max}
                 elif (M_2) then // M /\ M
                    let min = Math.Floor(Math.Min(i1.a*i2.b, i1.b*i2.a))
                    let max = Math.Ceiling(Math.Max(i1.a*i2.a, i1.b*i2.b))
                    {a = min; b = max}
                else // if (Z_2) M /\ Z
                    {a = 0.0; b = 0.0}

            elif (N1_1) then
                if (P1_2) then // N1 /\ P1
                    let min = floor i1.a*i2.b
                    let max = ceil i1.b*i2.a
                    {a = min; b = max}

                elif (P0_2) then // N1 /\ P0
                    let min = floor i1.a*i2.b
                    let max = 0.0; //ceil b*c);
                    {a = min; b = max}

                elif (M_2) then  // N1 /\ M
                    let min = floor i1.a*i2.b
                    let max = ceil i1.a*i2.a
                    {a = min; b = max}

                elif (N1_2)  then // N1 /\ N1
                    let min = floor i1.b*i2.b
                    let max = ceil i1.a*i2.a
                    {a = min; b = max}

                elif (N0_2) then // N1 /\ N0
                    let min = 0.0; //floor b*d);
                    let max = ceil i1.a*i2.a
                    {a = min; b = max}
                else// N1 /\ Z
                    {a = 0.0; b = 0.0}

            elif (N0_1) then
                if (P0_2 || P1_2) then // N0 /\ { P0 \/ P1}
                    let min = floor i1.a*i2.b
                    let max = 0.0; //ceil b*c);
                    {a = min; b = max}
                elif (N0_2 || N1_2) then // N0 /\ { N0 \/ N1}
                    let min = 0.0; //floor b*d);
                    let max = ceil i1.a*i2.a
                    {a = min; b = max}
                elif (M_2) then // N0 /\ M
                    let min = floor i1.a*i2.b
                    let max = ceil i1.a*i2.a
                    {a = min; b = max}
                else// N0 /\ Z
                     {a = 0.0; b = 0.0}
            else  //  Z /\ {ALL}
                {a = 0.0; b = 0.0}

        override this.Propagate (var : Variable) (allVars : Variable list) =
            let varX = allVars |> findVar x
            let varY = allVars |> findVar y
            let varZ = allVars |> findVar z

            printfn "%s * %s = %s; reducing the domain of %s" varX.Name varY.Name varZ.Name var.Name

            let oldResultMin = Double.NegativeInfinity
            let oldResultMax = Double.PositiveInfinity

            let reducedX = DivBounds(varZ.Domain, varY.Domain) <*> varX.Domain;
            let reducedY = DivBounds(varZ.Domain, varX.Domain) <*> varY.Domain;
            let reducedZ = MulBounds(varX.Domain, varY.Domain) <*> varZ.Domain;

            if var.Name = X then
                Variable(var.Name, reducedX)

            elif var.Name = Y then
               Variable(var.Name, reducedY)

            elif var.Name = Z then
                Variable(var.Name, reducedZ)
            else
                raise <| new ArgumentException(VAR_INVALID)

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
        member this.Split =
            let head = this.Variables.Head

            let half1 = Variable(head.Name, {a = head.Domain.a; b = head.Domain.Middle}) :: this.Variables.Tail
            let half2 = Variable(head.Name, {a = head.Domain.Middle; b = head.Domain.b}) :: this.Variables.Tail
            (Problem(this.Constraints, half1), Problem(this.Constraints, half2))

        member this.HasSolution = this.Variables.Length > 0