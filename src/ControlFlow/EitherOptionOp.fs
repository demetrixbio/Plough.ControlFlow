namespace Plough.ControlFlow.Operator.EitherOption

open Plough.ControlFlow

[<AutoOpen>]
module EitherOption =

    let inline (<!>) f x = EitherOption.map f x
    let inline (<*>) f x = EitherOption.apply f x
    let inline (<*^>) f x = EitherOption.apply f (Either.map Some x)
    let inline (>>=) x f = EitherOption.bind f x
