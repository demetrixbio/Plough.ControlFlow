namespace Plough.ControlFlow

open FSharp.Control.Tasks.Affine

[<RequireQualifiedAccess>]
module TaskEitherOption =
    let map (f : 'a -> 'b) (tro : TaskEither<'a option>) : TaskEither<'b option> = TaskEither.map (Option.map f) tro

    let bind (f : 'a -> TaskEither<'b option>) (tro : TaskEither<'a option>) : TaskEither<'b option> =
        let binder opt =
            match opt with
            | Some x -> f x
            | None -> TaskEither.retn None

        TaskEither.bind binder tro

    let map2 (f : 'a -> 'b -> 'c) (x : TaskEither<'a option>) (y : TaskEither<'b option>) : TaskEither<'c option> =
        TaskEither.map2 (Option.map2 f) x y

    let map3 (f : 'a -> 'b -> 'c -> 'd) (x : TaskEither<'a option>) (y : TaskEither<'b option>) (z : TaskEither<'c option>) : TaskEither<'d option> =
        TaskEither.map3 (Option.map3 f) x y z

    let retn (value : 'a) : TaskEither<'a option> =
        task { return Either.succeed (Some value) }

    let apply (f : TaskEither<('a -> 'b) option>) (x : TaskEither<'a option>) : TaskEither<'b option> =
        map2 (fun f x -> f x) f x

    /// Replaces the wrapped value with unit
    let ignore (tro : TaskEither<'a option>) : TaskEither<unit option> =
        tro |> map ignore