namespace Plough.ControlFlow

#if !FABLE_COMPILER
open System.Threading.Tasks
open FSharp.Control.Tasks
#endif

type FailureMessage =
    | Unknown of string
    | Database of string
    | Parse of string
    | Validation of string
    | Conflict of string
    | NotFound of string
    | ExceptionFailure of exn
    member this.Message =
        match this with
        | Unknown s
        | Database s
        | Parse s
        | Validation s
        | Conflict s
        | NotFound s -> s
        | ExceptionFailure e -> e.Message

    static member unwrap (failure : FailureMessage) =
        match failure with
        | FailureMessage.Database msg -> msg
        | FailureMessage.Parse msg -> msg
        | FailureMessage.Validation msg -> msg
        | FailureMessage.Conflict msg -> msg
        | FailureMessage.Unknown msg -> msg
        | FailureMessage.NotFound msg -> msg
        | FailureMessage.ExceptionFailure exn -> exn.ToString()

type Success<'TSuccess> =
    { Data : 'TSuccess
      Warnings : string list }

type Either<'a> = Result<Success<'a>, FailureMessage>

[<AutoOpen>]
module ActivePatterns =
    let (|Success|SuccessWithWarning|Failure|) answer =
        match answer with
        | Error e -> 
            Failure e
        | Ok { Data = d; Warnings = [] } -> 
            Success d
        | Ok { Data = d; Warnings = w } ->             
            SuccessWithWarning (d,w)

[<RequireQualifiedAccess>]
module Either =
    let fail : FailureMessage -> Either<'a> = Error
    let succeed x : Either<'a> = Ok { Data = x; Warnings = [] }
    let warn msg x : Either<'a> = Ok { Data = x; Warnings = [ msg ] }
    
    let isSuccess (x : Either<'a>) =
        match x with
        | Ok _ -> true
        | Error _ -> false

    let isFailure (x : Either<'a>) = isSuccess x |> not

    let bind (binder : 'a -> Either<'b>) (result : Either<'a>) : Either<'b> =
        match result with
        | Error e ->
            Error e
        | Ok { Data = d; Warnings = w1 } ->
            match binder d with
            | Error e -> 
                Error e
            | Ok { Data = d2; Warnings = w2 } ->
                // add back in the warnings
                Ok { Data = d2; Warnings = w1 @ w2 }
    
    let either (okF : 'a -> 'b) (errorF : FailureMessage -> 'b) (x : Either<'a>) : 'b =
        match x with
        | Ok { Data = x } -> okF x
        | Error err -> errorF err

    let eitherMap (okF : 'a -> 'b) (errorF : FailureMessage -> FailureMessage) (x : Either<'a>) : Either<'b> =
        either (okF >> succeed) (errorF >> fail) x

    let map (mapping : 'a -> 'b) (result : Either<'a>) : Either<'b> =
        match result with
        | Error e -> Error e
        | Ok { Data = d; Warnings = w } -> Ok { Data = mapping d; Warnings = w }
    
    let apply (f : Either<'a -> 'b>) (x : Either<'a>) : Either<'b> =
        match f, x with
        | Ok f', Ok x' -> Ok { Data = f'.Data x'.Data; Warnings = f'.Warnings @ x'.Warnings }
        | Error ef, Ok _  -> Error ef
        | Ok _, Error ex -> Error ex
        | Error (Validation ef | Parse ef), Error (Validation xf | Parse xf) -> [ef; xf] |> String.concat "\n" |> Validation |> Error
        | Error _, Error xf -> Error xf
    
    let map2 (f : 'a -> 'b -> 'c) (x : Either<'a>) (y : Either<'b>) : Either<'c> =
        (apply (apply (succeed f) x) y)

    let map3 (f : 'a -> 'b -> 'c -> 'd) (x : Either<'a>) (y : Either<'b>) (z : Either<'c>) : Either<'d> =
        apply (map2 f x y) z

    let fold (onSuccess : 'a -> 'b) (onFail : FailureMessage -> 'b) (r : Either<'a>) : 'b =
        match r with
        | Ok { Data = x }  -> onSuccess x
        | Error y -> onFail y

    /// Replaces the wrapped value with unit
    let ignore (result : Either<'a>) : Either<unit> = result |> map ignore

    /// Returns the specified error if the value is false.
    let isTrue (error : FailureMessage) (value : bool) : Either<unit> = if value then succeed () else fail error

    /// Returns the specified error if the value is true.
    let isFalse (error : FailureMessage) (value : bool) : Either<unit> = if not value then succeed () else fail error

    /// Converts an Option to an Either, using the given error if None.
    let isSome (error : FailureMessage) (option : 'a option) : Either<'a> =
        match option with
        | Some x -> succeed x
        | None -> fail error

    /// Converts an Option to an Either, using the given error if Some.
    let isNone (error : FailureMessage) (option : 'a option) : Either<unit> =
        match option with
        | Some _ -> fail error
        | None -> succeed ()

    /// Converts a nullable value into an Either, using the given error if null
    let isNotNull (error : FailureMessage) (value : 'a) : Either<'a> =
        match value with
        | null -> fail error
        | nonnull -> succeed nonnull

    /// Returns success if the two values are equal, or the specified error if not.
    /// Same as requireEqual, but with a signature that fits piping better than
    /// normal function application.
    let isEqualTo (other : 'a) (err : FailureMessage) (this : 'a) : Either<unit> =
        if this = other then succeed () else fail err

    /// Returns success if the two values are equal, or the specified error if not.
    /// Same as requireEqualTo, but with a signature that fits normal function
    /// application better than piping.
    let isEqual (x1 : 'a) (x2 : 'a) (error : FailureMessage) : Either<unit> =
        if x1 = x2 then succeed () else fail error

    /// Returns success if the sequence is empty, or the specified error if not.
    let isEmpty (error : FailureMessage) (xs : #seq<'a>) : Either<unit> =
        if Seq.isEmpty xs then succeed () else fail error

    /// Returns the specified error if the sequence is empty, or success if not.
    let isNotEmpty (error : FailureMessage) (xs : #seq<'a>) : Either<unit> =
        if Seq.isEmpty xs then fail error else succeed ()

    /// Returns the first item of the sequence if it exists, or the specified
    /// error if the sequence is empty
    let isHead (error : FailureMessage) (xs : #seq<'a>) : Either<'a> =
        match Seq.tryHead xs with
        | Some x -> succeed x
        | None -> fail error

    /// Replaces an error value with a custom error value.
    let setError (error : FailureMessage) (result : Either<'a>) : Either<'a> =
        result |> Result.mapError (fun _ -> error)

    /// Returns the contained value if success, otherwise returns ifError.
    let defaultValue (ifError : 'a) (result : Either<'a>) : 'a =
        match result with
        | Ok { Data = x } -> x
        | Error _ -> ifError

    /// Returns the contained value if success, otherwise evaluates ifErrorThunk and
    /// returns the result.
    let defaultWith (ifErrorThunk : unit -> 'a) (result : Either<'a>) : 'a =
        match result with
        | Ok { Data = x } -> x
        | Error _ -> ifErrorThunk ()

    /// Same as defaultValue for a result where the success value is unit. The name
    /// describes better what is actually happening in this case.
    let ignoreError (result : Either<unit>) : unit = defaultValue () result

    /// If the result is success and the predicate returns true, executes the function
    /// on the success value. Passes through the input value.
    let teeIf (predicate : 'a -> bool) (f : 'a -> unit) (result : Either<'a>) : Either<'a> =
        match result with
        | Ok { Data = x } -> if predicate x then f x
        | Error _ -> ()
        result

    /// If the result is FailureMessage and the predicate returns true, executes the
    /// function on the FailureMessage value. Passes through the input value.
    let teeErrorIf (predicate : FailureMessage -> bool) (f : FailureMessage -> unit) (result : Either<'a>) : Either<'a> =
        match result with
        | Ok _ -> ()
        | Error x -> if predicate x then f x
        result

    /// If the result is success, executes the function on the success value. Passes through
    /// the input value.
    let tee (f : 'a -> unit) (result : Either<'a>) : Either<'a> =
        teeIf (fun _ -> true) f result

    /// If the result is FailureMessage, executes the function on the FailureMessage value. Passes
    /// through the input value.
    let teeError (f : FailureMessage -> unit) (result : Either<'a>) : Either<'a> =
        teeErrorIf (fun _ -> true) f result

    
    
    /// Converts a Result<Task<_>,_> to an Task<Result<_,_>>
    let sequenceTask (resAsync : Either<Task<'a>>) : Task<Either<'a>> =
    #if !FABLE_COMPILER
        task {
    #else
        async {
    #endif
            match resAsync with
            | Ok promise ->
                let! x = promise.Data
                return Ok { Data = x; Warnings = promise.Warnings }
            | Error err -> return Error err
        }

    /// Returns the success value or runs the specified function over the failure value.
    let valueOr (f : FailureMessage -> 'a) (res : Either<'a>) : 'a =
        match res with
        | Ok { Data = x } -> x
        | Error x -> f x

    /// Takes two results and returns a tuple of the pair
    let zip (x : Either<'a>) (y : Either<'b>) : Either<'a * 'b> =
        match x, y with
        | Ok { Data = x'; Warnings = xw }, Ok { Data = y'; Warnings = yw } -> Ok { Data = x', y'; Warnings = xw @ yw }
        | Error e, _ -> Error e
        | _, Error e -> Error e

    let ofChoice (choice2map : 'b -> FailureMessage) (c : Choice<'a, 'b>) : Either<'a> =
        match c with
        | Choice1Of2 x -> succeed x
        | Choice2Of2 x -> fail (choice2map x)
        
    let inline onError (onError : FailureMessage -> Either<'b>) (f : unit -> Either<'b>) : Either<'b> =
        match f() with
        | Success _ | SuccessWithWarning _ as s -> s
        | Failure s -> onError s     