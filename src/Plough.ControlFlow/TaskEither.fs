namespace Plough.ControlFlow

#if !FABLE_COMPILER 
open FSharp.Control.Tasks.Affine
#endif

type TaskEither<'a> = Task<Either<'a>>

[<RequireQualifiedAccess>]
module TaskEither =
    let fail (x : FailureMessage) : TaskEither<'a> =
        x |> Either.fail |> Task.singleton
    
    let succeed (x : 'a) : TaskEither<'a> =
        x |> Either.succeed |> Task.singleton
    
    let warn (msg : string) (x : 'a) : TaskEither<'a> =
        Either.warn msg x |> Task.singleton
    
    let map (f : 'a -> 'b) (te : TaskEither<'a>) : TaskEither<'b> =
        Task.map (Either.map f) te

    let bind (f : 'a -> TaskEither<'b>) (te : TaskEither<'a>) : TaskEither<'b> =
    #if !FABLE_COMPILER
        task {
    #else
        async {
    #endif
            match! te with
            | Error e ->
                return Error e
            | Ok { Data = d; Warnings = w1 } ->
                match! f d with
                | Error e -> 
                    return Error e
                | Ok { Data = d2; Warnings = w2 } ->
                    // add back in the warnings
                    return Ok { Data = d2; Warnings = w1 @ w2 }
        }

    let foldResult (onSuccess : 'a -> 'b) (onError : FailureMessage -> 'b) (te : TaskEither<'a>) : Task<'b> =
        Task.map (Either.fold onSuccess onError) te

    let ofTask (aTask : Task<'a>) : TaskEither<'a> =
        aTask
        |> Task.map Either.succeed
    
    let ofAsync (aAsync : Async<'a>) : TaskEither<'a> =
    #if !FABLE_COMPILER
        aAsync
        |> Async.Catch
        |> Async.StartAsTask
        |> Task.map (Either.ofChoice ExceptionFailure)
    #else
        aAsync |> ofTask
    #endif

    let retn (x : 'a) : TaskEither<'a> =
        Either.succeed x |> Task.singleton

    let returnError (x : FailureMessage) : TaskEither<'a> =
        Either.fail x |> Task.singleton

    let map2 (f : 'a -> 'b -> 'c) (x : TaskEither<'a>) (y : TaskEither<'b>) : TaskEither<'c> =
        Task.map2 (Either.map2 f) x y

    let map3 (f : 'a -> 'b -> 'c -> 'd) (x : TaskEither<'a>) (y : TaskEither<'b>) (z : TaskEither<'c>) : TaskEither<'d> =
        Task.map3 (Either.map3 f) x y z

    let apply (f : TaskEither<'a -> 'b>) (x : TaskEither<'a>) : TaskEither<'b> =
        map2 (fun f x -> f x) f x

    /// Replaces the wrapped value with unit
    let ignore (tr : TaskEither<'a>) : TaskEither<unit> =
        tr |> map ignore

    /// Returns the specified error if the task-wrapped value is false.
    let isTrue (error : FailureMessage) (value : Task<bool>) : TaskEither<unit> =
        value |> Task.map (Either.isTrue error)

    /// Returns the specified error if the task-wrapped value is true.
    let isFalse (error : FailureMessage) (value : Task<bool>) : TaskEither<unit> =
        value |> Task.map (Either.isFalse error)

    // Converts an task-wrapped Option to an Either, using the given error if None.
    let isSome (error : FailureMessage) (option : Task<'a option>) : TaskEither<'a> =
        option |> Task.map (Either.isSome error)

    // Converts an task-wrapped Option to an Either, using the given error if Some.
    let isNone (error : FailureMessage) (option : Task<'a option>) : TaskEither<unit> =
        option |> Task.map (Either.isNone error)

    let isAny (f : Task<'a>) : TaskEither<unit> =
        f |> Task.map (fun _ -> Either.succeed ())
    
    /// Returns success if the task-wrapped value and the provided value are equal, or the specified error if not.
    let isEqual (x1 : 'a) (x2 : Task<'a>) (error : FailureMessage) : TaskEither<unit> =
        x2
        |> Task.map (fun x2' -> Either.isEqual x1 x2' error)

    /// Returns success if the two values are equal, or the specified error if not.
    let isEqualTo (other : 'a) (error : FailureMessage) (this : Task<'a>) : TaskEither<unit> =
        this
        |> Task.map (Either.isEqualTo other error)

    /// Returns success if the task-wrapped sequence is empty, or the specified error if not.
    let isEmpty (error : FailureMessage) (xs : Task<'a>) : TaskEither<unit> =
        xs |> Task.map (Either.isEmpty error)

    /// Returns success if the task-wrapped sequence is not-empty, or the specified error if not.
    let isNotEmpty (error : FailureMessage) (xs : Task<'a>) : TaskEither<unit> =
        xs |> Task.map (Either.isNotEmpty error)

    /// Returns the first item of the task-wrapped sequence if it exists, or the specified
    /// error if the sequence is empty
    let isHead (error : FailureMessage) (xs : Task<'a>) : TaskEither<'a> =
        xs |> Task.map (Either.isHead error)

    /// Replaces an error value of an task-wrapped result with a custom error
    /// value.
    let setError (error : FailureMessage) (taskEither : TaskEither<'a>) : TaskEither<'a> =
        taskEither |> Task.map (Either.setError error)

    /// Extracts the contained value of an task-wrapped result if success, otherwise
    /// uses ifError.
    let defaultValue (ifError : 'a) (taskEither : TaskEither<'a>) : Task<'a> =
        taskEither
        |> Task.map (Either.defaultValue ifError)

    /// Extracts the contained value of an task-wrapped result if success, otherwise
    /// evaluates ifErrorThunk and uses the result.
    let defaultWith (ifErrorThunk : unit -> 'a) (taskEither : TaskEither<'a>) : Task<'a> =
        taskEither
        |> Task.map (Either.defaultWith ifErrorThunk)

    /// Same as defaultValue for a result where the success value is unit. The name
    /// describes better what is actually happening in this case.
    let ignoreError (taskEither : TaskEither<unit>) : Task<unit> =
        defaultValue () taskEither

    /// If the task-wrapped result is success, executes the function on the success value.
    /// Passes through the input value.
    let tee (f : 'a -> unit) (taskEither : TaskEither<'a>) : TaskEither<'a> =
        taskEither |> Task.map (Either.tee f)

    /// If the task-wrapped result is success and the predicate returns true, executes
    /// the function on the success value. Passes through the input value.
    let teeIf (predicate : 'a -> bool) (f : 'a -> unit) (taskEither : TaskEither<'a>) : TaskEither<'a> =
        taskEither |> Task.map (Either.teeIf predicate f)

    /// If the task-wrapped result is Error, executes the function on the Error
    /// value. Passes through the input value.
    let teeError f (taskEither : TaskEither<'a>) : TaskEither<'a> =
        taskEither |> Task.map (Either.teeError f)

    /// If the task-wrapped result is Error and the predicate returns true,
    /// executes the function on the Error value. Passes through the input value.
    let teeErrorIf (predicate : FailureMessage -> bool) (f : FailureMessage -> unit) (taskEither : TaskEither<'a>) : TaskEither<'a> =
        taskEither
        |> Task.map (Either.teeErrorIf predicate f)

    /// Takes two results and returns a tuple of the pair
    let zip (x1 : TaskEither<'a>) (x2 : TaskEither<'b>) : TaskEither<'a * 'b> =
        Task.zip x1 x2
        |> Task.map (fun (r1, r2) -> Either.zip r1 r2)
        
    let onError (onError : FailureMessage -> TaskEither<'b>) (f : TaskEither<'b>) : TaskEither<'b> =
        try
            f |> Task.bind (fun f ->
                match f with
                | Success _ | SuccessWithWarning _ as s -> s |> Task.singleton
                | Failure s -> onError s)
        with
        | exn ->
            exn |> FailureMessage.ExceptionFailure |> onError
            
    #if !FABLE_COMPILER
    let toTask f = f |> foldResult id (fun error -> error.ToString() |> failwith) :> System.Threading.Tasks.Task
    
    let runSynchronously (f: TaskEither<_>) = f.ConfigureAwait(false).GetAwaiter().GetResult()
    
    #else
    let runSynchronously (f: TaskEither<_>) = f |> Async.RunSynchronously
    
    #endif
    
    let unwrap (te : TaskEither<'a>) =
        match te |> runSynchronously with
        | Ok r -> r.Data 
        | Error e -> e |> FailureMessage.unwrap |> failwith