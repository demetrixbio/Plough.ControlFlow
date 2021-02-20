namespace Plough.ControlFlow.Operator.Task

open Plough.ControlFlow

[<AutoOpen>]
module Task =
    let inline (<!>) f x = Task.map f x
    let inline (<*>) f x = Task.apply f x
    let inline (>>=) x f = Task.bind f x
