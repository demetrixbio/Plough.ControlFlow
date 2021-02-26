namespace Plough.ControlFlow

#if !FABLE_COMPILER 
open FSharp.Control.Tasks.Affine
#endif

[<RequireQualifiedAccess>]
module TaskOption =

    let inline map f ar = Task.map (Option.map f) ar

    let bind f (ar : Task<_>) =
        #if !FABLE_COMPILER 
        task {
        #else
        async {
        #endif
            let! opt = ar

            let t =
                match opt with
                | Some x -> f x
                | None -> None |> Task.singleton

            return! t
        }

    let retn x = Some x |> Task.singleton

    let apply f x =
        bind (fun f' -> bind (fun x' -> retn (f' x')) x) f
