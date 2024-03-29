﻿namespace Plough.ControlFlow

open System

[<AutoOpen>]
module EitherCE =
    
    type EitherBuilder() =
        member __.Return(value : 'a) : Either<'a> = Either.succeed value

        member inline __.ReturnFrom(result : Either<'a>) : Either<'a> = result

        member this.Zero() : Either<unit> = this.Return ()

        member inline __.Bind(result : Either<'a>, binder : 'a -> Either<'b>) : Either<'b> =
            Either.bind binder result

        member __.Delay(generator : unit -> Either<'a>) : unit -> Either<'a> = generator

        member inline __.Run(generator : unit -> Either<'a>) : Either<'a> = generator ()

        member this.Combine(result : Either<unit>, binder : unit -> Either<'a>) : Either<'a> =
            this.Bind(result, binder)

        member this.TryWith(generator : unit -> Either<'a>, handler : exn -> Either<'a>) : Either<'a> =
            try
                this.Run generator
            with e -> handler e

        member this.TryFinally(generator : unit -> Either<'a>, compensation : unit -> unit) : Either<'a> =
            try
                this.Run generator
            finally
                compensation ()

        member this.Using(resource : 'a :> IDisposable, binder : 'a -> Either<'b>) : Either<'b> =
            this.TryFinally
                ((fun () -> binder resource),
                 (fun () ->
                     if not <| obj.ReferenceEquals(resource, null)
                     then resource.Dispose()))

        member _.While(guard : unit -> bool, computation : unit -> Either<unit>) : Either<unit> =
            let mutable fin, result = false, Either.succeed ()

            while not fin && guard () do
                match computation () with
                | Ok { Data = (); Warnings = w1 } ->
                    match result with
                    | Error _ as e ->
                        result <- e
                        fin <- true
                    | Ok { Data = (); Warnings = w2 } ->
                        result <- Ok { Data = (); Warnings = w1 @ w2 }
                | Error _ as e ->
                    result <- e
                    fin <- true

            result

        member _.For(sequence : #seq<'a>, binder : 'a -> Either<unit>) : Either<unit> =
            use enumerator = sequence.GetEnumerator()
            let mutable fin, result = false, Either.succeed ()

            while not fin && enumerator.MoveNext() do
                match binder enumerator.Current with
                | Ok { Data = (); Warnings = w1 } ->
                    match result with
                    | Error _ as e ->
                        result <- e
                        fin <- true
                    | Ok { Data = (); Warnings = w2 } ->
                        result <- Ok { Data = (); Warnings = w1 @ w2 }
                | Error _ as e ->
                    result <- e
                    fin <- true

            result


        member _.BindReturn(x : Either<'a>, f) = Either.map f x

        member _.MergeSources(t1 : Either<'a>, t2 : Either<'b>) = Either.zip t1 t2

        /// <summary>
        /// Method lets us transform data types into our internal representation.  This is the identity method to recognize the self type.
        ///
        /// See https://stackoverflow.com/questions/35286541/why-would-you-use-builder-source-in-a-custom-computation-expression-builder
        /// </summary>
        /// <param name="result"></param>
        /// <returns></returns>
        member inline _.Source(result : Either<'a>) : Either<'a> = result

    let either = EitherBuilder()

// Having these members as extensions gives them lower priority in
// overload resolution and allows skipping more type annotations.
[<AutoOpen>]
module EitherCEExtensions =

    type EitherBuilder with
        /// <summary>
        /// Needed to allow `for..in` and `for..do` functionality
        /// </summary>
        member inline __.Source(s : #seq<'a>) = s
        
        /// <summary>
        /// Method lets us transform data types into our internal representation.
        /// </summary>
        /// <returns></returns>
        member inline _.Source(choice : Choice<_,FailureMessage>) = Either.ofChoice id choice
        
        
    open System.Collections.Generic
            
    [<AbstractClass>]
    type Either() =
        static member inline collect (source : (unit -> Either<'a>) seq) : Either<'a seq> =
            either {
                let results = List()
                
                for item in source do
                    let! result = item ()
                    results.Add(result)
                
                return upcast results
            }
            
        static member inline collectMany (source : (unit -> Either<'a seq>) seq) : Either<'a seq> =
            either {
                let results = List()
                
                for item in source do
                    let! result = item ()
                    results.AddRange(result)
                
                return upcast results
            }
        
        static member inline collect (source : (unit -> Either<'a>) list) : Either<'a list> =
            either {
                let mutable results = List.empty
                
                for item in source do
                    let! result = item ()
                    results <- results @ [result]
                
                return results
            }
            
        static member inline collectMany (source : (unit -> Either<'a list>) list) : Either<'a list> =
            either {
                let mutable results = List.empty
                
                for item in source do
                    let! result = item ()
                    results <- results @ result
                
                return results
            }

        static member inline collect (source : (unit -> Either<'a>) []) : Either<'a []> = 
            either {
                let results = Array.init source.Length (fun _ -> Unchecked.defaultof<'a>)
                
                for i in 0 .. source.Length - 1 do
                    let! result = source.[i] ()
                    results.[i] <- result
                
                return results
            }
            
        static member inline collectMany (source : (unit -> Either<'a []>) []) : Either<'a []> = 
            either {
                let results = List()
                
                for item in source do
                    let! result = item ()
                    results.AddRange(result)
                
                return results.ToArray()
            }