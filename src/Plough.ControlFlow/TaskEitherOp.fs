namespace Plough.ControlFlow.Operator.TaskResult

open Plough.ControlFlow

[<AutoOpen>]
module TaskEither =

    let inline (<!>) f x = TaskEither.map f x
    let inline (<*>) f x = TaskEither.apply f x
    let inline (>>=) x f = TaskEither.bind f x
