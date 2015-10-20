namespace ConstraintSolver

open System

/// A constraint.
type Constraint(expression:string) =
    member this.Expression = expression

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