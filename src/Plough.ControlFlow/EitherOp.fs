namespace Plough.ControlFlow.Operator.Either

open Plough.ControlFlow

[<AutoOpen>]
module Either =
    let inline (<!>) f x = Either.map f x
    let inline (<*>) f x = Either.apply f x
    let inline (>>=) x f = Either.bind f x