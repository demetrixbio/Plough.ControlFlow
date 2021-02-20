namespace Plough.ControlFlow

[<RequireQualifiedAccess>]
module EitherOption =

    let map (f : 'a -> 'b) (ro : Either<'a option>) : Either<'b option> =
        Either.map (Option.map f) ro

    let bind (f : 'a -> Either<'b option>) (ro : Either<'a option>) : Either<'b option> =
        Either.bind (function
            | Some x -> f x
            | None -> Either.succeed None) ro

    let retn (x : 'a) : Either<'a option> =
        Either.succeed (Some x)

    let apply (f : Either<('a -> 'b) option>) (x : Either<'a option>) : Either<'b option> =
        bind (fun f' -> bind (fun x' -> retn (f' x')) x) f

    let map2 (f : 'a -> 'b -> 'c) (x : Either<'a option>) (y : Either<'b option>) : Either<'c option> =
        (apply (apply (retn f) x) y)

    let map3 (f : 'a -> 'b -> 'c -> 'd) (x : Either<'a option>) (y : Either<'b option>) (z : Either<'c option>) : Either<'d option> =
        apply (map2 f x y) z

    /// Replaces the wrapped value with unit
    let ignore (ro : Either<'a option>) : Either<unit option> =
        ro |> map ignore
