namespace Plough.ControlFlow

open System

[<AutoOpen>]
module TaskOptionCE =
    type TaskOptionBuilder() =
        #if !FABLE_COMPILER
        member inline _.Return(value : 'T) : Task<Option<_>> =
            value |> Some |> Task.FromResult

        member inline _.ReturnFrom(taskResult : Task<Option<_>>) : Task<Option<_>> =
            taskResult

        member inline this.ReturnFrom(asyncResult : Async<Option<_>>) : Task<Option<_>> =
            asyncResult |> Async.StartAsTask

        member inline _.ReturnFrom(result : Option<_>) : Task<Option<_>> =
            result |> Task.FromResult

        member inline _.Zero() : Task<Option<_>> =
            option.Zero() |> Task.FromResult

        member inline _.Bind(taskResult : Task<Option<_>>, binder : 'T -> Task<Option<_>>) : Task<Option<_>> =
            task {
                match! taskResult with
                | None -> return None
                | Some d1 ->
                    match! binder d1 with
                    | None -> return None
                    | Some d2 -> return d2
            }

        member inline this.Bind(asyncResult : Async<Option<_>>, binder : 'T -> Task<Option<_>>) : Task<Option<_>> =
            this.Bind(Async.StartAsTask asyncResult, binder)

        member inline this.Bind(result : Option<_>, binder : 'T -> Task<Option<_>>) : Task<Option<_>> =
            let result = result |> Task.FromResult
            this.Bind(result, binder)

        member inline _.Delay(generator : unit -> Task<Option<_>>) =
            generator

        member inline _.Combine(computation1 : Task<Option<'T>>, computation2 : unit -> Task<Option<'U>>)
                                : Task<Option<'U>> =
            task {
                match! computation1 with
                | None -> return None
                | Some _ -> return! computation2 ()
            }

        member inline _.TryWith(computation : unit -> Task<Option<_>>, handler : exn -> Task<Option<_>>) : Task<Option<_>> =
            task {
                try
                    return! computation ()
                with
                | ex ->
                    return! handler ex
            }

        member inline _.TryFinally(computation : unit -> Task<Option<_>>, compensation : unit -> unit) : Task<Option<_>> =
            task {
                try
                    return! computation ()
                finally
                    compensation ()
            }

        member inline _.Using(resource : 'T :> IDisposable, binder : 'T -> Task<Option<_>>) : Task<Option<_>> =
            task {
                use res = resource
                let! result = binder res
                return result
            }

        member _.While(guard : unit -> bool, computation : unit -> Task<Option<'U>>) : Task<Option<'U>> =
            task {
                let mutable fin, result = false, None

                while not fin && guard () do
                    match! computation () with
                    | Some _ as o -> result <- o
                    | None ->
                        result <- None
                        fin <- true

                return result
            }

        member _.For(sequence : #seq<'T>, binder : 'T -> Task<Option<'U>>) : Task<Option<'U>> =
            task {
                use enumerator = sequence.GetEnumerator()
                let mutable fin, result = false, None

                while not fin && enumerator.MoveNext() do
                    match! binder enumerator.Current with
                    | Some _ as o -> result <- o
                    | None ->
                        result <- None
                        fin <- true

                return result
            }

        member inline _.Run(f : unit -> Task<'m>) = f ()
        
        #else
        
        member inline _.Return(value : 'T) : Task<Option<_>> = async.Return <| option.Return value

        member inline _.ReturnFrom(taskResult : Task<Option<_>>) : Task<Option<_>> = taskResult

        member inline _.ReturnFrom(result : Option<_>) : Task<Option<_>> = async.Return result

        member inline _.Zero() : Task<Option<_>> = async.Return <| option.Zero()

        member inline _.Bind(taskResult : Task<Option<_>>, binder : 'T -> Task<Option<_>>) : Task<Option<_>> =
            let binder' r =
                match r with
                | Some x -> binder x
                | None -> async.Return None

            async.Bind(taskResult, binder')

        member inline this.Bind(result : Option<_>, binder : 'T -> Task<Option<_>>) : Task<Option<_>> =
            let result = result |> Task.singleton
            this.Bind(result, binder)

        member inline _.Delay(generator : unit -> Task<Option<_>>) = async.Delay(generator)

        member inline _.Combine(computation1 : Task<Option<'T>>, computation2 : unit -> Task<Option<'U>>)
                                : Task<Option<'U>> =
            async {
                match! computation1 with
                | None -> return None
                | Some _ -> return! computation2 ()
            }

        member inline _.TryWith(computation : unit -> Task<Option<_>>, handler : exn -> Task<Option<_>>)
                                : Task<Option<_>> =
            async.TryWith(computation(), handler)

        member inline _.TryFinally(computation : unit -> Task<Option<_>>, compensation : unit -> unit) : Task<Option<_>> =
            async.TryFinally(computation(), compensation)

        member inline _.Using(resource : 'T :> IDisposable, binder : 'T -> Task<Option<_>>) : Task<Option<_>> =
            async.Using(resource, binder)

        member _.While(guard : unit -> bool, computation : unit -> Task<Option<'U>>) : Task<Option<'U>> =
            async {
                let mutable fin, result = false, None

                while not fin && guard () do
                    match! computation () with
                    | Some _ as o -> result <- o
                    | None ->
                        result <- None
                        fin <- true

                return result
            }

        member _.For(sequence : #seq<'T>, binder : 'T -> Task<Option<'U>>) : Task<Option<'U>> =
            async {
                use enumerator = sequence.GetEnumerator()
                let mutable fin, result = false, None

                while not fin && enumerator.MoveNext() do
                    match! binder enumerator.Current with
                    | Some _ as o -> result <- o
                    | None ->
                        result <- None
                        fin <- true

                return result
            }

        member inline _.Run(f : unit -> Task<'m>) = async.ReturnFrom (f())
        
        #endif

    let taskOption = TaskOptionBuilder()
