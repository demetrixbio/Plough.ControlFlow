namespace Plough.ControlFlow

open System.Threading.Tasks
open FSharp.Control.Tasks.Affine

#if FABLE_COMPILER
    type Task<'T> = System.Threading.Tasks.Task<'T>
#else
    type Task<'T> = Async<'T>
    type Ply<'T> = Async<'T>
#endif

[<RequireQualifiedAccess>]
module Task =
    #if FABLE_COMPILER
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
    
    let ofUnit (t : Task) = task { return! t }    
        
    #else
    
    let singleton value = async { return value }
    
    let bind (f : 'a -> Task<'b>) (x : Task<'a>) =
        async {
            let! x = x
            return! f x
        }
    
    /// Takes two tasks and returns a tuple of the pair
    let zip (a1 : Task<_>) (a2 : Task<_>) =
        async {
            let! r1 = a1
            let! r2 = a2
            return r1, r2
        }

    let ignore (x : Task<'a>) : Task<unit> =
        async {
            let! _ = x
            ()
        }
    
    #endif
    
    let apply f x =
        bind (fun f' -> bind (fun x' -> singleton (f' x')) x) f

    let map f x = x |> bind (f >> singleton)

    let map2 f x y = (apply (apply (singleton f) x) y)

    let map3 f x y z = apply (map2 f x y) z


