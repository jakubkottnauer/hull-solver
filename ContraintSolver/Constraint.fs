namespace ConstraintSolver

open System

module Constraint =

    [<AbstractClass>]
    type T(expression:string) =
        member this.Expression = expression

        abstract member Propagate : string -> Variable list -> Variable

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
    type VarPlusVarEqConstConstraint(x: string, y: string, c: decimal) =
        inherit T(x + " + " + y + " = " + c.ToString())
        member this.X = x
        member this.Y = y
        member this.C = c

        override this.Propagate (v : string) (allVars : Variable list) =
            let varX = List.find (fun (item:Variable) -> item.Name = "x") allVars
            let varY = List.find (fun (item:Variable) -> item.Name = "y") allVars

            let XplusY = varX.Domain + varY.Domain
            let CminusX = this.C - varX.Domain
            let CminusY = this.C - varY.Domain
            let narrowedX = XplusY.Intersect varX.Domain
            let narrowedY = CminusX.Intersect varY.Domain

            match v with // TODO: Fix hardcoded variable names - right now only "x" and "y" are supported.
            | "x" ->
                Variable(v, narrowedX)
            | "y" ->
                Variable(v, narrowedY)