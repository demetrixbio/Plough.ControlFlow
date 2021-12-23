namespace Plough.ControlFlow

[<AutoOpen>]
module TaskEitherOptionCE =

    type TaskEitherOptionBuilder() =
        #if !FABLE_COMPILER
        member inline _.Return(value : 'a) : Task<Either<'a option>> =
            value |> Some |> Either.succeed |> Task.FromResult

        member inline _.ReturnFrom(taskEither : TaskEither<'a option>) : Task<Either<'a option>> =
            taskEither

        member inline _.Bind(taskEither : TaskEither<'a option>, binder : 'a -> Task<Either<'b option>>) : Task<Either<'b option>> =
            task {
                match! taskEither with
                | Error e ->
                    return Error e
                | Ok { Data = None; Warnings = w } -> return Ok { Data = None; Warnings = w }
                | Ok { Data = (Some x); Warnings = w1 } ->
                    match! binder x with
                    | Error e -> return Error e
                    | Ok { Data = y; Warnings = w2 } -> return Ok { Data = y; Warnings = w1 @ w2 }
            }

        member inline _.Combine(tro1, tro2) =
            tro1
            |> TaskEitherOption.bind (fun _ -> tro2)
            |> Task.FromResult

        member inline _.Delay (generator : unit -> Task<Either<Option<'a>>>) : unit -> Task<Either<Option<'a>>> =
            generator

        member inline _.Run(f : unit -> Task<'m>) = f ()
        #else
        
        member inline _.Return(value : 'a) : Task<Either<'a option>> = async.Return <| either.Return(Some value)

        member inline _.ReturnFrom(taskEither : TaskEither<'a option>) : Task<Either<'a option>> =
            async.ReturnFrom taskEither

        member inline _.Bind(taskEither : TaskEither<'a option>, binder : 'a -> Task<Either<'b option>>) : Task<Either<'b option>> =
            let binder' r =
                async {
                    match r with
                    | Ok { Data = (Some x); Warnings = w } ->
                        match! binder x with
                        | Ok { Data = y; Warnings = yw } -> return Ok { Data = y; Warnings = w @ yw }
                        | Error x -> return Error x
                    | Ok { Data = None; Warnings = w } -> return Ok { Data = None; Warnings = w }
                    | Error x -> return Either.fail x
                }

            async.Bind(taskEither, binder')

        member inline _.Combine(tro1, tro2) =
            tro1
            |> TaskEitherOption.bind (fun _ -> tro2)
            |> async.ReturnFrom

        member inline _.Delay f = async.Delay f

        member inline _.Run(f : unit -> Task<'m>) = async.ReturnFrom (f())
        
        #endif

    let taskEitherOption = TaskEitherOptionBuilder()
