module Plough.ControlFlow.Tests.EitherTests

open Plough.ControlFlow
open Plough.ControlFlow.Tests.ErrorSpecs
open Xunit


let SuccessWithSuccess =
    either {
        let! x = 2 |> failIfTooBig 10
        return! x |> failIfTooBig 10
    }

let SuccessWithWarning =
    either {
        let! x = 4 |> failIfTooBig 10
        return! x |> warnIfTooBig 7
    }

let SuccessWithFailure =
    either {
        let! x = 5 |> failIfTooBig 10
        return! x |> failIfTooBig 9 // again
    }

let WarningWithFailure =
    either {
        let! x = 6 |> warnIfTooBig 5
        return! x |> failIfTooBig 10
    }

let WarningWithSuccess =
    either {
        let! x = 6 |> warnIfTooBig 5
        return! x |> failIfTooBig 15
    }

let WarningWithWarning =
    either {
        let! x = 6 |> warnIfTooBig 5
        let! y = x |> failIfTooBig 20
        return! y |> warnIfTooBig 10 // again
    }


[<Fact>]
let ``success, success -> success``(): unit =
    let actual = Ok {Data = 8; Warnings = [];}
    let result = SuccessWithSuccess
    
    //2 + 2 => 4 & 4 + 4 => 8 < 10
    Assert.Equal(actual, result)

[<Fact>]
let ``success, success with warning -> success with warning``(): unit =
    let actual = Ok { Data = 16; Warnings = ["Your number is higher than 7 warning"] }
    let result = SuccessWithWarning
    
    //4 + 4 => 8 + 8 => 16 > 7
    Assert.Equal(actual, result)
    
[<Fact>]
let ``success, failure -> failure``(): unit =
    let actual = Error <| Validation "Cannot be bigger than 9"
    let result = SuccessWithFailure

    //5 + 5 => 10 (> 9) + 10 => 20
    Assert.Equal(actual, result)
    
[<Fact>]
let ``success with warning, failure -> failure``(): unit =
    let actual = Error <| Validation "Cannot be bigger than 10"
    let result = WarningWithFailure

    //6 + 6 => 12 (> 10) + 12 => 24
    Assert.Equal(actual, result)
    
[<Fact>]
let ``success with warning, success -> success with warning``(): unit =
    let actual = Ok { Data = 24; Warnings = ["Your number is higher than 5 warning"] }
    let result = WarningWithSuccess

    //6 + 6 => 12 (> 5) + 12 => 24
    Assert.Equal(actual, result)
    
[<Fact>]
let ``success with warning, success with warning -> success with 2 warnings``(): unit =
    let actual = Ok { Data = 48; Warnings = ["Your number is higher than 5 warning"; "Your number is higher than 10 warning"] }
    let result = WarningWithWarning
    Assert.Equal(actual, result)