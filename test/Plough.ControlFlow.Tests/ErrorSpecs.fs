module Plough.ControlFlow.Tests.ErrorSpecs

open Plough.ControlFlow

let failIfTooBig max number: Result<_, _> =
    if number > max then
        Validation (sprintf "Cannot be bigger than %i" max) |> Either.fail
    else
        number + number |> Either.succeed

let warnIfTooBig max number =
    if number >= max then
        let temp = number + number
        temp |> Either.warn (sprintf "Your number is higher than %i warning" max)
    else
        number + number |> Either.succeed