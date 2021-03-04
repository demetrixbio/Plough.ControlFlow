namespace Plough.ControlFlow.Operator.TaskEitherOption

open Plough.ControlFlow

[<AutoOpen>]
module TaskEitherOption = 

  let inline (<!>) f x = TaskEitherOption.map f x
  let inline (<*>) f x = TaskEitherOption.apply f x
  let inline (>>=) x f = TaskEitherOption.bind f x