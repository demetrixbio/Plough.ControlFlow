namespace Plough.ControlFlow.Operator.TaskOption

open Plough.ControlFlow

[<AutoOpen>]
module TaskOption =

    let inline (<!>) f x = TaskOption.map f x
    let inline (<*>) f x = TaskOption.apply f x
    let inline (>>=) x f = TaskOption.bind f x
