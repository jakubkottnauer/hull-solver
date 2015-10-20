namespace ConstraintSolver

open System

module Constraint =

    [<AbstractClass>]
    type T(expression:string) =
        member this.Expression = expression

        abstract member Propagate : Variable -> Variable

        override this.Equals(y) =
            match y with
            | :? T as other -> (this.Expression = other.Expression)
            | _ -> false

        override x.GetHashCode() = hash x.Expression

        interface IComparable with
          member x.CompareTo ycons =
              match ycons with
              | :? T as other -> compare x.Expression other.Expression
              | _ -> 0

    /// A "x + y = c" constraint.
    type VarPlusVarEqConstConstraint(x: Variable, y: Variable, c: decimal) =
        inherit T(x.Name + " + " + y.Name + " = " + c.ToString())
        member this.X = x
        member this.Y = y
        member this.C = c

        override this.Propagate v : Variable =
            let XplusY = this.X.Domain + this.Y.Domain
            let CminusX = this.C - this.X.Domain
            let CminusY = this.C - this.Y.Domain
            let narrowedX = XplusY.Intersect this.X.Domain
            let narrowedY = CminusX.Intersect this.Y.Domain
            
            // TODO: Update variables in this constraint with the narrowed values.

            match v.Name with // TODO: Fix hardcoded variable names - right now only "x" and "y" are supported.
            | "x" ->
                Variable(v.Name, narrowedX)
            | "y" ->
                Variable(v.Name, narrowedY)

    /// A "x - y = c" constraint.
    type VarMinusVarEqConstConstraint(x: Variable, y: Variable, c: decimal) =
        inherit T(x.Name + " - " + y.Name + " = " + c.ToString())

        override this.Propagate v : Variable =
            v