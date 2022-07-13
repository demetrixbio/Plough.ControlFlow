module Plough.ControlFlow.Tests.TaskEitherTests

open Plough.ControlFlow
open Plough.ControlFlow.Tests.ErrorSpecs
open Xunit

let SuccessWithSuccess =
    taskEither {
        let! x = 2 |> failIfTooBig 10
        return! x |> failIfTooBig 10
    }

let SuccessWithWarning =
    taskEither {
        let! x = 4 |> failIfTooBig 10
        return! x |> warnIfTooBig 7
    }

let SuccessWithFailure =
    taskEither {
        let! x = 5 |> failIfTooBig 10
        return! x |> failIfTooBig 9 // again
    }

let WarningWithFailure =
    taskEither {
        let! x = 6 |> warnIfTooBig 5
        return! x |> failIfTooBig 10
    }

let WarningWithSuccess =
    taskEither {
        let! x = 6 |> warnIfTooBig 5
        return! x |> failIfTooBig 15
    }

let WarningWithWarning =
    taskEither {
        let! x = 6 |> warnIfTooBig 5
        let! y = x |> failIfTooBig 20
        return! y |> warnIfTooBig 10 // again
    }


[<Fact>]
let ``success, success -> success``() = task {
    let actual = Ok {Data = 8; Warnings = [];}
    let! result = SuccessWithSuccess
    
    //2 + 2 => 4 & 4 + 4 => 8 < 10
    Assert.Equal(actual, result)
}

[<Fact>]
let ``success, success with warning -> success with warning``() = task {
    let actual = Ok { Data = 16; Warnings = ["Your number is higher than 7 warning"] }
    let! result = SuccessWithWarning
    
    //4 + 4 => 8 + 8 => 16 > 7
    Assert.Equal(actual, result)
}
    
[<Fact>]
let ``success, failure -> failure``() = task {
    let actual = Error <| Validation "Cannot be bigger than 9"
    let! result = SuccessWithFailure

    //5 + 5 => 10 (> 9) + 10 => 20
    Assert.Equal(actual, result)
}
    
[<Fact>]
let ``success with warning, failure -> failure``() = task {
    let actual = Error <| Validation "Cannot be bigger than 10"
    let! result = WarningWithFailure

    //6 + 6 => 12 (> 10) + 12 => 24
    Assert.Equal(actual, result)
}
    
[<Fact>]
let ``success with warning, success -> success with warning``() = task {
    let actual = Ok { Data = 24; Warnings = ["Your number is higher than 5 warning"] }
    let! result = WarningWithSuccess

    //6 + 6 => 12 (> 5) + 12 => 24
    Assert.Equal(actual, result)
}
    
[<Fact>]
let ``success with warning, success with warning -> success with 2 warnings``() = task {
    let actual = Ok { Data = 48; Warnings = ["Your number is higher than 5 warning"; "Your number is higher than 10 warning"] }
    let! result = WarningWithWarning
    Assert.Equal(actual, result)
}