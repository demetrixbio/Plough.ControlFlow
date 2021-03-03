namespace Plough.DynamicQuery

open System
open Plough.ControlFlow

type PageQuery() =
    member val PageSize = 50L with get, set
    member val PageIndex = 0L with get, set

type SortDirection =
    | Ascending
    | Descending
    member this.Toggle() =
        match this with
        | Ascending -> Descending
        | Descending -> Ascending

[<RequireQualifiedAccess>]
module SortDirection =
    let toString = function
        | SortDirection.Ascending -> ""
        | SortDirection.Descending -> "desc"

type ResourceList<'a> =
    { Items : 'a list
      PageSize : int64
      PageIndex : int64
      TotalItemCount : int64 option }
   
module ResourceList =
    let map f resourceList =
        { Items = resourceList.Items |> List.map f
          PageSize = resourceList.PageSize
          PageIndex = resourceList.PageIndex
          TotalItemCount = resourceList.TotalItemCount }
   
    #if !FABLE_COMPILER

    // 'valueAsSucceed' is not handled by fable compiler, so given function is excluded from frontend
    let rec private batchImpl
                        (getPage: int64 -> int64 -> Async<ResourceList<'a>>)
                        pageIndex
                        pageSize
                        (processPage: 'a list -> TaskEither<unit>) =
        taskEither {
            let! currentPage = getPage pageIndex pageSize |> Async.StartAsTask
            do! currentPage.Items |> processPage
            if int64 currentPage.Items.Length = currentPage.PageSize then
                return! batchImpl getPage (pageIndex + 1L) pageSize processPage
        }

    let batch getPage pageSize processPage = batchImpl getPage 0L pageSize processPage

    #endif
    
type Condition = And | Or

type IPredicate = interface end
and Filter<'t when 't :> IPredicate> =
    | Simple of 't
    | Complex of Complex<'t>
    | Empty
    
and Complex<'t when 't :> IPredicate> =
    { Condition : Condition 
      Predicates : Filter<'t> list }
    
[<RequireQualifiedAccess>]
module Operation =
    
    type 'a Algebraic =
        | Equals of 'a
        | GreaterThen of 'a
        | LessThen of 'a
        | GreaterThenOrEquals of 'a
        | LessThenOrEquals of 'a
    
    type Int = int Algebraic    
    type Decimal = decimal Algebraic    
    type Timestamp = DateTime Algebraic    
    type String = | Equals of string | Like of string
    type Bool = | Equals of bool
    type 'a Category = | Equals of 'a
    type 'a Set = | Any of 'a [] | Neither of 'a []
    
type [<AbstractClass>] Constraint<'t, 'op, 'predicate when 'predicate :> IPredicate>() =
    abstract member Apply : 'op -> 'predicate

type [<AbstractClass>] IntConstraint<'predicate when 'predicate :> IPredicate>() =
    inherit Constraint<int, Operation.Int, 'predicate>()
    member x.Equals (value : int) = Operation.Algebraic.Equals value |> x.Apply |> Simple
    member x.GreaterThan (value : int) = Operation.Algebraic.GreaterThen value |> x.Apply |> Simple
    member x.LessThan (value : int) = Operation.Algebraic.LessThen value |> x.Apply |> Simple
    member x.GreaterThanOrEquals (value : int) = Operation.Algebraic.GreaterThenOrEquals value |> x.Apply |> Simple
    member x.LessThanOrEquals (value : int) = Operation.Algebraic.LessThenOrEquals value |> x.Apply |> Simple

type [<AbstractClass>] DecimalConstraint<'predicate when 'predicate :> IPredicate>() =
    inherit Constraint<decimal, Operation.Decimal, 'predicate>()
    member x.Equals (value : decimal) = Operation.Algebraic.Equals value |> x.Apply |> Simple
    member x.GreaterThan (value : decimal) = Operation.Algebraic.GreaterThen value |> x.Apply |> Simple
    member x.LessThan (value : decimal) = Operation.Algebraic.LessThen value |> x.Apply |> Simple
    member x.GreaterThanOrEquals (value : decimal) = Operation.Algebraic.GreaterThenOrEquals value |> x.Apply |> Simple
    member x.LessThanOrEquals (value : decimal) = Operation.Algebraic.LessThenOrEquals value |> x.Apply |> Simple

type [<AbstractClass>] TimestampConstraint<'predicate when 'predicate :> IPredicate>() =
    inherit Constraint<DateTime, Operation.Timestamp, 'predicate>()
    member x.Equals (value : DateTime) = Operation.Algebraic.Equals value |> x.Apply |> Simple
    member x.GreaterThan (value : DateTime) = Operation.Algebraic.GreaterThen value |> x.Apply |> Simple
    member x.LessThan (value : DateTime) = Operation.Algebraic.LessThen value |> x.Apply |> Simple
    member x.GreaterThanOrEquals (value : DateTime) = Operation.Algebraic.GreaterThenOrEquals value |> x.Apply |> Simple
    member x.LessThanOrEquals (value : DateTime) = Operation.Algebraic.LessThenOrEquals value |> x.Apply |> Simple

type [<AbstractClass>] StringConstraint<'predicate when 'predicate :> IPredicate>() =
    inherit Constraint<string, Operation.String, 'predicate>()
    member x.Equals (value : string) = Operation.String.Equals value |> x.Apply |> Simple
    member x.Like (value : string) = Operation.String.Like value |> x.Apply |> Simple

type [<AbstractClass>] BoolConstraint<'predicate when 'predicate :> IPredicate>() =
    inherit Constraint<bool, Operation.Bool, 'predicate>()
    member x.Equals(value : bool) = Operation.Bool.Equals value |> x.Apply |> Simple

type [<AbstractClass>] CategoryConstraint<'a, 'predicate when 'predicate :> IPredicate>() =
    inherit Constraint<'a, 'a Operation.Category, 'predicate>()
    member x.Equals(value : 'a) = Operation.Category.Equals value |> x.Apply |> Simple
    
type [<AbstractClass>] SetConstraint<'a, 'predicate when 'predicate :> IPredicate>() =
    inherit Constraint<'a, 'a Operation.Set, 'predicate>()
    member x.Any (value : 'a []) = Operation.Set.Any value |> x.Apply |> Simple
    member x.Neither (value : 'a []) = Operation.Set.Neither value |> x.Apply |> Simple