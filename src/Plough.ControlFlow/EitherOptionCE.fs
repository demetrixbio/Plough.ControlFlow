namespace Plough.ControlFlow

[<AutoOpen>]
module EitherOptionCE =

    type EitherOptionBuilder() =
        member __.Return value = EitherOption.retn value
        member __.ReturnFrom value = value

        member __.Bind(resultOpt, binder) = EitherOption.bind binder resultOpt

        member __.Combine(r1, r2) = r1 |> EitherOption.bind (fun _ -> r2)

        member __.Delay f = f ()

    let eitherOption = EitherOptionBuilder()
