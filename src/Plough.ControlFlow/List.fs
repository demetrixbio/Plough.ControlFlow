namespace Plough.ControlFlow

[<RequireQualifiedAccess>]
module List =

    let rec private traverseEither' (state : Either<'b list>) (f : 'a -> Either<'b>) (xs : 'a list) : Either<'b list> =
        match xs with
        | [] -> state
        | x :: xs ->
            let r =
                either {
                    let! y = f x
                    let! ys = state
                    return ys @ [ y ]
                }

            match r with
            | Ok _ -> traverseEither' r f xs
            | Error _ -> r

    let traverseEither (f : 'a -> Either<'b>) (xs : 'a list) : Either<'b list> =
        traverseEither' (Either.succeed []) f xs

    let private traverseTaskEither' (f : 'a -> TaskEither<'b>) (xs : 'a list) : TaskEither<'b list> =
        let mutable state = Either.succeed []
        let mutable index = 0
        let xs = xs |> List.toArray
        #if !FABLE_COMPILER
        task {
        #else
        async {
        #endif
            while state |> Either.isSuccess && index < xs.Length do
                let! r = xs |> Array.item index |> f
                index <- index + 1

                match (r, state) with
                | Ok y, Ok ys ->
                    state <- Ok { Data = y.Data::ys.Data; Warnings = y.Warnings @ ys.Warnings }
                | Error e, _ ->
                    state <- Error e
                | _, _ -> ()

            return state |> Either.map List.rev
        }

    let traverseTaskEither (f : 'a -> TaskEither<'b>) (xs : 'a list) : TaskEither<'b list> =
        traverseTaskEither' f xs