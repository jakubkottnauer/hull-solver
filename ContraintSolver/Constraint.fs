namespace ConstraintSolver

open System

module Constraint =

    [<AbstractClass>]
    type T(expression:string, variableNames: string list) =
        member this.Expression = expression

        member this.VariableNames = variableNames

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
    type VarPlusVarEqVarConstraint(x: string, y: string, z: string) =
        inherit T(x + " + " + y + " = " + z, [x; y; z])

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
                raise <| new ArgumentException("Invalid variable")