namespace Plough.ControlFlow

open Fable.Core

#if !FABLE_COMPILER
type Task<'T> = System.Threading.Tasks.Task<'T>
#else
type Task<'T> = Async<'T>
#endif

// Ply Shim for task CE override to remove collision with F# 6 native task CE
#if !FABLE_COMPILER
[<AutoOpen>]
module TaskBuilder =
    let task = Microsoft.FSharp.Control.TaskBuilder.task
#else
    let task = Microsoft.FSharp.Core.ExtraTopLevelOperators.async
#endif

[<RequireQualifiedAccess>]
module Task =
    let singleton value = value |> Task.FromResult

    let bind (f : 'a -> Task<'b>) (x : Task<'a>) =
        task {
            let! x = x
            return! f x
        }
    
    /// Takes two tasks and returns a tuple of the pair
    let zip (a1 : Task<_>) (a2 : Task<_>) =
        task {
            let! r1 = a1
            let! r2 = a2
            return r1, r2
        }

    let ignore (x : Task<'a>) =
        task {
            let! _ = x
            ()
        }
    
    #if !FABLE_COMPILER
    let ofUnit (t : System.Threading.Tasks.Task) = task { return! t }
    #endif
    
    let apply f x =
        bind (fun f' -> bind (fun x' -> singleton (f' x')) x) f

    let map f x = x |> bind (f >> singleton)

    let map2 f x y = (apply (apply (singleton f) x) y)

    let map3 f x y z = apply (map2 f x y) z

    let asAsync (task: Task<_>) =
        task
        #if !FABLE_COMPILER
        |> Async.AwaitTask
        #endif
        
    let startAsPromise (task: Task<_>) = task |> asAsync |> Async.StartAsPromise
