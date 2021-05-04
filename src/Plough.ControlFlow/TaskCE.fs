namespace Plough.ControlFlow

#if !FABLE_COMPILER
open FSharp.Control.Tasks.Affine.Unsafe
open FSharp.Control.Tasks.Affine
#endif

[<AutoOpen>]
module TaskCE =

    open System.Collections.Generic
            
    [<AbstractClass>]
    type Task() =
        static member inline collect (source : (unit -> Task<'a>) seq) : Task<'a seq> =
            task {
                let results = List()
                
                for item in source do
                    let! result = item ()
                    results.Add(result)
                
                return upcast results
            }
            
        static member inline collectMany (source : (unit -> Task<'a seq>) seq) : Task<'a seq> =
            task {
                let results = List()
                
                for item in source do
                    let! result = item ()
                    results.AddRange(result)
                
                return upcast results
            }
        
        static member inline collect (source : (unit -> Task<'a>) list) : Task<'a list> =
            task {
                let mutable results = List.empty
                
                for item in source do
                    let! result = item ()
                    results <- results @ [result]
                
                return results
            }
            
        static member inline collectMany (source : (unit -> Task<'a list>) list) : Task<'a list> =
            task {
                let mutable results = List.empty
                
                for item in source do
                    let! result = item ()
                    results <- results @ result
                
                return results
            }

        static member inline collect (source : (unit -> Task<'a>) []) : Task<'a []> = 
            task {
                let results = Array.init source.Length (fun _ -> Unchecked.defaultof<'a>)
                
                for i in 0 .. source.Length - 1 do
                    let! result = source.[i] ()
                    results.[i] <- result
                
                return results
            }
            
        static member inline collectMany (source : (unit -> Task<'a []>) []) : Task<'a []> = 
            task {
                let results = List()
                
                for item in source do
                    let! result = item ()
                    results.AddRange(result)
                
                return results.ToArray()
            }