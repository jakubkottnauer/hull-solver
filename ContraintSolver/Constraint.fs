namespace ConstraintSolver

open System

module Constraint =

    [<AbstractClass>]
    type T(expression:string) =
        member this.Expression = expression

        abstract member Propagate : Variable -> Variable list -> Variable

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

    /// A "x + y = z" constraint.
    type VarPlusVarEqVarConstraint(x: string, y: string, c: string) =
        inherit T(x + " + " + y + " = " + c)
        member this.X = x
        member this.Y = y
        member this.C = c

        override this.Propagate (pair : Variable) (allVars : Variable list) =
            let varX = List.find (fun (item:Variable) -> item.Name = "x") allVars
            let varY = List.find (fun (item:Variable) -> item.Name = "y") allVars
            let varZ = List.find (fun (item:Variable) -> item.Name = "z") allVars

            let XplusY = varX.Domain + varY.Domain
            let ZminusX = varZ.Domain - varX.Domain
            let ZminusY = varZ.Domain - varY.Domain
            let narrowedX = ZminusY.Intersect varX.Domain
            let narrowedY = ZminusX.Intersect varY.Domain
            let narrowedZ = XplusY.Intersect varZ.Domain

            match pair.Name with // TODO: Fix hardcoded variable names - right now only "x" and "y" are supported.
            | "x" ->
                Variable(pair.Name, narrowedX)
            | "y" ->
                Variable(pair.Name, narrowedY)
            | "z" ->
                Variable(pair.Name, narrowedZ)