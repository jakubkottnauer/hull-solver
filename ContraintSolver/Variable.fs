namespace ConstraintSolver

open System

/// A variable
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