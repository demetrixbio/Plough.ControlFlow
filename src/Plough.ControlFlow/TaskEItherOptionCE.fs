namespace Plough.ControlFlow

open FSharp.Control.Tasks.Affine.Unsafe
open FSharp.Control.Tasks.Affine
open Ply

[<AutoOpen>]
module TaskEitherOptionCE =

    type TaskEitherOptionBuilder() =
        member inline _.Return(value : 'a) : Ply<Either<'a option>> = uply.Return <| either.Return(Some value)

        member inline _.ReturnFrom(taskEither : TaskEither<'a option>) : Ply<Either<'a option>> =
            uply.ReturnFrom taskEither

        member inline _.Bind(taskEither : TaskEither<'a option>, binder : 'a -> Ply<Either<'b option>>) : Ply<Either<'b option>> =
            let binder' r =
                uply {
                    match r with
                    | Ok { Data = (Some x); Warnings = w } ->
                        match! binder x with
                        | Ok { Data = y; Warnings = yw } -> return Ok { Data = y; Warnings = w @ yw }
                        | Error x -> return Error x
                    | Ok { Data = None; Warnings = w } -> return Ok { Data = None; Warnings = w }
                    | Error x -> return Either.fail x
                }

            uply.Bind(taskEither, binder')

        member inline _.Combine(tro1, tro2) =
            tro1
            |> TaskEitherOption.bind (fun _ -> tro2)
            |> uply.ReturnFrom

        member inline _.Delay f = uply.Delay f

        member inline _.Run(f : unit -> Ply<'m>) = task.Run f

    let taskEitherOption = TaskEitherOptionBuilder()
