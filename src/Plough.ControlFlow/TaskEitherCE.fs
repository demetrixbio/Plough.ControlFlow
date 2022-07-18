namespace Plough.ControlFlow

open System

[<AutoOpen>]
module TaskEitherCE =

    type TaskEitherBuilderFableShim() =

        member inline _.MergeSources(t1 : Task<Either<'a>>, t2 : Task<Either<'b>>) = TaskEither.zip t1 t2
        
        /// <summary>
        /// Method lets us transform data types into our internal representation. This is the identity method to recognize the self type.
        ///
        /// See https://stackoverflow.com/questions/35286541/why-would-you-use-builder-source-in-a-custom-computation-expression-builder
        /// </summary>
        member inline _.Source(task : Task<Either<'a>>) : Task<Either<'a>> = task
        
        #if !FABLE_COMPILER
        
        #else
        member inline _.Return(value : 'a) : Async<Either<'a>> =
            async.Return(either.Return value)

        member inline _.ReturnFrom(taskEither : TaskEither<'a>) : Async<Either<'a>> =
            async.ReturnFrom taskEither

        member inline _.Zero() : Async<Either<unit>> = async.Return <| either.Zero()

        member inline _.Bind(taskEither : TaskEither<'a>, binder : 'a -> Async<Either<'b>>) : Async<Either<'b>> =
            async {
                match! taskEither with
                | Error e ->
                    return Error e
                | Ok { Data = d; Warnings = w1 } ->
                    match! binder d with
                    | Error e -> return Error e
                    | Ok { Data = d2; Warnings = w2 } -> return Ok { Data = d2; Warnings = w1 @ w2 }
            }

        member inline _.Delay(generator : unit -> Async<Either<'a>>) : unit -> Async<Either<'a>> =
            fun () -> async.Delay(generator)

        member inline _.Combine(computation1 : Async<Either<unit>>, computation2 : unit -> Async<Either<'a>>) : Async<Either<'a>> =
            async {
                match! computation1 with
                | Error e -> return Error e
                | Ok  { Data = _; Warnings = w1 } ->
                    match! computation2 () with
                    | Error e -> return Error e
                    | Ok { Data = d2; Warnings = w2 } -> return Ok { Data = d2; Warnings = w1 @ w2 }
            }

        member inline _.TryWith(computation : unit -> Async<Either<'a>>, handler : exn -> Async<Either<'a>>) : Async<Either<'a>> =
            async.TryWith(computation(), handler)

        member inline _.TryFinally(computation : unit -> Async<Either<'a>>, compensation : unit -> unit) : Async<Either<'a>> =
            async.TryFinally(computation(), compensation)

        member inline _.Using(resource : 'a :> IDisposable, binder : 'a -> Async<Either<'b>>) : Async<Either<'b>> =
            async.Using(resource, binder)

        member _.While(guard : unit -> bool, computation : unit -> Async<Either<unit>>) : Async<Either<unit>> =
            async {
                let mutable fin, result = false, Either.succeed ()

                while not fin && guard () do
                    match! computation () with
                    | Ok { Data = (); Warnings = w1 } ->
                        match result with
                        | Error _ -> ()
                        | Ok { Data = (); Warnings = w2 } ->
                            result <- Ok { Data = (); Warnings = w1 @ w2 }
                    | Error _ as e ->
                        result <- e
                        fin <- true

                return result
            }

        member _.For(sequence : #seq<'a>, binder : 'a -> Async<Either<unit>>) : Async<Either<unit>> =
            async {
                use enumerator = sequence.GetEnumerator()
                let mutable fin, result = false, Either.succeed ()

                while not fin && enumerator.MoveNext() do
                    match! binder enumerator.Current with
                    | Ok { Data = (); Warnings = w1 } ->
                        match result with
                        | Error _ -> ()
                        | Ok { Data = (); Warnings = w2 } ->
                            result <- Ok { Data = (); Warnings = w1 @ w2 }
                    | Error _ as e ->
                        result <- e
                        fin <- true

                return result
            }

        member inline this.BindReturn(x : Task<Either<'a>>, f) =
            this.Bind(x, (fun x -> this.Return(f x)))
            
        member inline _.Run(f : unit -> Async<'m>) = async.ReturnFrom (f())
        #endif
        
    

// Having members as extensions gives them lower priority in
// overload resolution between Task<'a> and Task<Either<'a>>.
[<AutoOpen>]
module TaskEitherCEExtensions =
    type TaskEitherBuilderFableShim with
        /// <summary>
        /// Needed to allow `for..in` and `for..do` functionality
        /// </summary>
        member inline _.Source(s : #seq<'a>) = s

        /// <summary>
        /// Method lets us transform data types into our internal representation.
        /// </summary>
        member inline _.Source(result : Either<'a>) : Task<Either<'a>> = Task.singleton result

        /// <summary>
        /// Method lets us transform data types into our internal representation.
        /// </summary>
        member inline _.Source(choice : Choice<'a, FailureMessage>) : Task<Either<'a>> =
            choice |> Either.ofChoice id |> Task.singleton

        #if !FABLE_COMPILER
        /// <summary>
        /// Method lets us transform data types into our internal representation.
        /// </summary>
        member inline _.Source(asyncComputation : Async<'a>) : Task<Either<'a>> =
            asyncComputation
            |> Async.StartAsTask
            |> Task.map Either.succeed
        #endif

        /// <summary>
        /// Method lets us transform data types into our internal representation.
        /// </summary>
        member inline _.Source(task : Task<'a>) : Task<Either<'a>> =
            task |> Task.map Either.succeed

        #if !FABLE_COMPILER
        /// <summary>
        /// Method lets us transform data types into our internal representation.
        /// </summary>
        member inline _.Source(t : System.Threading.Tasks.Task) : Task<Either<unit>> =
            task { return! t } |> Task.map Either.succeed
        #endif
        
          


#if !FABLE_COMPILER 
            

open System
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Control
open Microsoft.FSharp.Collections

[<Struct; NoComparison; NoEquality>]
type TaskEitherStateMachineData<'T> =

    [<DefaultValue(false)>]
    val mutable Result: Either<'T>

    [<DefaultValue(false)>]
    val mutable Warnings: ResizeArray<string>

    [<DefaultValue(false)>]
    val mutable MethodBuilder: AsyncTaskEitherMethodBuilder<'T>
    member this.IsResultError = 
        
        Either.isFailure this.Result
    member this.IsTaskCompleted = this.MethodBuilder.Task.IsCompleted
    member this.AddWarnings s = 
        if isNull this.Warnings then
            this.Warnings <- ResizeArray<string>()
        this.Warnings.AddRange s
    member this.GetWarningsAsList =
        if isNull this.Warnings then []
        else List.ofSeq this.Warnings


and AsyncTaskEitherMethodBuilder<'TOverall> = AsyncTaskMethodBuilder<Either<'TOverall>>
and TaskEitherStateMachine<'TOverall> = ResumableStateMachine<TaskEitherStateMachineData<'TOverall>>
and TaskEitherResumptionFunc<'TOverall> = ResumptionFunc<TaskEitherStateMachineData<'TOverall>>

and TaskEitherResumptionDynamicInfo<'TOverall> =
    ResumptionDynamicInfo<TaskEitherStateMachineData<'TOverall>>

and TaskEitherCode<'TOverall, 'T> = ResumableCode<TaskEitherStateMachineData<'TOverall>, 'T>

module TaskEitherBuilderBase =

    let rec WhileDynamic
        (
            sm: byref<TaskEitherStateMachine<_>>,
            condition: unit -> bool,
            body: TaskEitherCode<_, _>
        ) : bool =
        if condition () then
            if body.Invoke(&sm) then
                if sm.Data.IsResultError then
                    // Set the result now to allow short-circuiting of the rest of the CE.
                    // Run/RunDynamic will skip setting the result if it's already been set.
                    // Combine/CombineDynamic will not continue if the result has been set.
                    sm.Data.MethodBuilder.SetResult sm.Data.Result
                    true
                else
                    WhileDynamic(&sm, condition, body)
            else
                let rf = sm.ResumptionDynamicInfo.ResumptionFunc

                sm.ResumptionDynamicInfo.ResumptionFunc <-
                    (TaskEitherResumptionFunc<_>(fun sm -> WhileBodyDynamicAux(&sm, condition, body, rf)))

                false
        else
            true

    and WhileBodyDynamicAux
        (
            sm: byref<TaskEitherStateMachine<_>>,
            condition: unit -> bool,
            body: TaskEitherCode<_, _>,
            rf: TaskEitherResumptionFunc<_>
        ) : bool =
        if rf.Invoke(&sm) then
            if sm.Data.IsResultError then
                // Set the result now to allow short-circuiting of the rest of the CE.
                // Run/RunDynamic will skip setting the result if it's already been set.
                // Combine/CombineDynamic will not continue if the result has been set.
                sm.Data.MethodBuilder.SetResult sm.Data.Result
                true
            else
                WhileDynamic(&sm, condition, body)
        else
            let rf = sm.ResumptionDynamicInfo.ResumptionFunc

            sm.ResumptionDynamicInfo.ResumptionFunc <-
                (TaskEitherResumptionFunc<_>(fun sm -> WhileBodyDynamicAux(&sm, condition, body, rf)))

            false

type TaskEitherBuilderBase() =
    member inline _.Delay
        (generator: unit -> TaskEitherCode<'TOverall, 'T>)
        : TaskEitherCode<'TOverall, 'T> =
        TaskEitherCode<'TOverall, 'T>(fun sm -> (generator ()).Invoke(&sm))

    /// Used to represent no-ops like the implicit empty "else" branch of an "if" expression.
    [<DefaultValue>]
    member inline _.Zero<'TOverall>() : TaskEitherCode<'TOverall, unit> = 
        TaskEitherCode<_, _>(fun sm ->
            // printfn "Return Called --> "
            sm.Data.Result <- Either.succeed Unchecked.defaultof<_>
            true)

    member inline _.Return(value: 'T) : TaskEitherCode<_,_> =
        TaskEitherCode<'T, _>(fun sm ->
            // printfn "Return Called --> "
            // if not sm.Data.IsResultError then
            sm.Data.Result <- Ok { Data = value; Warnings = sm.Data.GetWarningsAsList }
            true)

    static member inline CombineDynamic
        (
            sm: byref<TaskEitherStateMachine<_>>,
            task1: TaskEitherCode<'TOverall, unit>,
            task2: TaskEitherCode<'TOverall, 'T>
        ) : bool =
        let shouldContinue = task1.Invoke(&sm)

        if sm.Data.IsTaskCompleted then
            true
        elif sm.Data.IsResultError then 
            true
        elif shouldContinue then
            task2.Invoke(&sm)
        else
            let rec resume (mf: TaskEitherResumptionFunc<_>) =
                TaskEitherResumptionFunc<_>(fun sm ->
                    let shouldContinue = mf.Invoke(&sm)
                    
                    if sm.Data.IsTaskCompleted then
                        true
                    elif sm.Data.IsResultError then 
                        true
                    elif shouldContinue then
                        task2.Invoke(&sm)
                    else
                        sm.ResumptionDynamicInfo.ResumptionFunc <- (resume (sm.ResumptionDynamicInfo.ResumptionFunc))

                        false)

            sm.ResumptionDynamicInfo.ResumptionFunc <- (resume (sm.ResumptionDynamicInfo.ResumptionFunc))
            false

    /// Chains together a step with its following step.
    /// Note that this requires that the first step has no result.
    /// This prevents constructs like `task { return 1; return 2; }`.
    member inline _.Combine
        (
            task1: TaskEitherCode<'TOverall, unit>,
            task2: TaskEitherCode<'TOverall, 'T>
        ) : TaskEitherCode<'TOverall, 'T> =

        TaskEitherCode<'TOverall, 'T>(fun sm ->
            if __useResumableCode then
                //-- RESUMABLE CODE START
                // NOTE: The code for code1 may contain await points! Resuming may branch directly
                // into this code!
                // printfn "Combine Called Before Invoke --> "
                let __stack_fin = task1.Invoke(&sm)
                // printfn "Combine Called After Invoke --> %A " sm.Data.MethodBuilder.Task.Status

                if sm.Data.IsTaskCompleted then true
                elif sm.Data.IsResultError then true
                elif __stack_fin then task2.Invoke(&sm)
                else false
            else
                TaskEitherBuilderBase.CombineDynamic(&sm, task1, task2))



    /// Builds a step that executes the body while the condition predicate is true.
    member inline _.While
        (
            [<InlineIfLambda>] condition: unit -> bool,
            body: TaskEitherCode<'TOverall, unit>
        ) : TaskEitherCode<'TOverall, unit> =
        TaskEitherCode<'TOverall, unit>(fun sm ->
            if __useResumableCode then
                //-- RESUMABLE CODE START
                let mutable __stack_go = true

                while __stack_go
                      && not sm.Data.IsResultError
                      && condition () do
                    // NOTE: The body of the state machine code for 'while' may contain await points, so resuming
                    // the code will branch directly into the expanded 'body', branching directly into the while loop
                    let __stack_body_fin = body.Invoke(&sm)
                    // printfn "While After Invoke --> %A" sm.Data.Result
                    // If the body completed, we go back around the loop (__stack_go = true)
                    // If the body yielded, we yield (__stack_go = false)
                    __stack_go <- __stack_body_fin

                if sm.Data.IsResultError then
                    // Set the result now to allow short-circuiting of the rest of the CE.
                    // Run/RunDynamic will skip setting the result if it's already been set.
                    // Combine/CombineDynamic will not continue if the result has been set.
                    sm.Data.MethodBuilder.SetResult sm.Data.Result

                __stack_go
            //-- RESUMABLE CODE END
            else
                TaskEitherBuilderBase.WhileDynamic(&sm, condition, body))

    /// Wraps a step in a try/with. This catches exceptions both in the evaluation of the function
    /// to retrieve the step, and in the continuation of the step (if any).
    member inline _.TryWith
        (
            body: TaskEitherCode<'TOverall, 'T>,
            catch: exn -> TaskEitherCode<'TOverall, 'T>
        ) : TaskEitherCode<'TOverall, 'T> =

        // printfn "TryWith Called --> "
        ResumableCode.TryWith(body, catch)

    /// Wraps a step in a try/finally. This catches exceptions both in the evaluation of the function
    /// to retrieve the step, and in the continuation of the step (if any).
    member inline _.TryFinally
        (
            body: TaskEitherCode<'TOverall, 'T>,
            [<InlineIfLambda>] compensation: unit -> unit
        ) : TaskEitherCode<'TOverall, 'T> =

        // printfn "TryFinally Called --> "

        ResumableCode.TryFinally(
            body,
            ResumableCode<_, _>(fun _sm ->
                compensation ()
                true)
        )

    member inline this.For
        (
            sequence: seq<'T>,
            body: 'T -> TaskEitherCode<'TOverall, unit>
        ) : TaskEitherCode<'TOverall, unit> =
        // A for loop is just a using statement on the sequence's enumerator...
        ResumableCode.Using(
            sequence.GetEnumerator(),
            // ... and its body is a while loop that advances the enumerator and runs the body on each element.
            (fun e ->
                this.While(
                    (fun () -> e.MoveNext()),
                    TaskEitherCode<'TOverall, unit>(fun sm -> (body e.Current).Invoke(&sm))
                ))
        )
#if NETSTANDARD2_1
    member inline internal this.TryFinallyAsync
        (
            body: TaskEitherCode<'TOverall, 'T>,
            compensation: unit -> ValueTask
        ) : TaskEitherCode<'TOverall, 'T> =
        ResumableCode.TryFinallyAsync(
            body,
            ResumableCode<_, _>(fun sm ->
                if __useResumableCode then
                    let mutable __stack_condition_fin = true
                    let __stack_vtask = compensation ()

                    if not __stack_vtask.IsCompleted then
                        let mutable awaiter = __stack_vtask.GetAwaiter()
                        // printfn "TryFinallyAsync Before Invoke Task.Status --> %A" sm.Data.MethodBuilder.Task.Status
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_condition_fin <- __stack_yield_fin
                        // printfn "TryFinallyAsync Task.Status --> %A" sm.Data.MethodBuilder.Task.Status

                        if not __stack_condition_fin then
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)

                    __stack_condition_fin
                else
                    let vtask = compensation ()
                    let mutable awaiter = vtask.GetAwaiter()

                    let cont =
                        TaskEitherResumptionFunc<'TOverall>(fun sm ->
                            awaiter.GetResult() |> ignore
                            true)

                    // shortcut to continue immediately
                    if awaiter.IsCompleted then
                        true
                    else
                        sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                        sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                        false)
        )


    member inline this.Using<'Resource, 'TOverall, 'T, 'Error when 'Resource :> IAsyncDisposable>
        (
            resource: 'Resource,
            body: 'Resource -> TaskEitherCode<'TOverall, 'Error, 'T>
        ) : TaskEitherCode<'TOverall, 'Error, 'T> =
        this.TryFinallyAsync(
            (fun sm -> (body resource).Invoke(&sm)),
            (fun () ->
                if not (isNull (box resource)) then
                    resource.DisposeAsync()
                else
                    ValueTask())
        )
#endif

    member inline this.Source(taskEither: TaskEither<'T>) : TaskEither<'T> = taskEither
    member inline _.Source(result: Async<Result<_, _>>) : Task<Result<_, _>> = result |> Async.StartAsTask
    #if NETSTANDARD2_1
    member inline _.Source(t: ValueTask<Result<_, _>>) : Task<Result<_, _>> = task { return! t }
    #endif
    member inline _.Source(result: Result<_, _>) : Task<Result<_, _>> = Task.singleton result

    member inline _.Source(result: Choice<_, _>) : Task<Either<_>> =
        result |> Either.ofChoice id |> Task.singleton




type TaskEitherBuilder() =

    inherit TaskEitherBuilderBase()

    // This is the dynamic implementation - this is not used
    // for statically compiled tasks.  An executor (resumptionFuncExecutor) is
    // registered with the state machine, plus the initial resumption.
    // The executor stays constant throughout the execution, it wraps each step
    // of the execution in a try/with.  The resumption is changed at each step
    // to represent the continuation of the computation.
    static member RunDynamic(code: TaskEitherCode<'T, 'T>) : TaskEither<'T> =
        let mutable sm = TaskEitherStateMachine<'T>()

        let initialResumptionFunc =
            TaskEitherResumptionFunc<'T>(fun sm -> code.Invoke(&sm))

        let resumptionInfo =
            { new TaskEitherResumptionDynamicInfo<_>(initialResumptionFunc) with
                member info.MoveNext(sm) =
                    let mutable savedExn = null

                    try
                        sm.ResumptionDynamicInfo.ResumptionData <- null
                        // printfn "RunDynamic BeforeInvoke Data --> %A" sm.Data.Result
                        let step = info.ResumptionFunc.Invoke(&sm)
                        // printfn "RunDynamic AfterInvoke Data --> %A %A" sm.Data.Result sm.Data.MethodBuilder.Task.Status

                        // If the `sm.Data.MethodBuilder` has already been set somewhere else (like While/WhileDynamic), we shouldn't continue
                        if sm.Data.IsTaskCompleted then
                            ()
                        elif step then
                            // printfn "RunDynamic Data --> %A" sm.Data.Result
                            sm.Data.MethodBuilder.SetResult(sm.Data.Result)
                        else
                            let mutable awaiter =
                                sm.ResumptionDynamicInfo.ResumptionData :?> ICriticalNotifyCompletion

                            assert not (isNull awaiter)
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)

                    with
                    | exn -> savedExn <- exn
                    // Run SetException outside the stack unwind, see https://github.com/dotnet/roslyn/issues/26567
                    match savedExn with
                    | null -> ()
                    | exn -> sm.Data.MethodBuilder.SetException exn

                member _.SetStateMachine(sm, state) =
                    sm.Data.MethodBuilder.SetStateMachine(state)
            }

        sm.ResumptionDynamicInfo <- resumptionInfo
        sm.Data.MethodBuilder <- AsyncTaskEitherMethodBuilder<'T>.Create ()
        sm.Data.MethodBuilder.Start(&sm)
        sm.Data.MethodBuilder.Task

    member inline _.Run(code: TaskEitherCode<'T, 'T>) : TaskEither<'T> =
        if __useResumableCode then
            __stateMachine<TaskEitherStateMachineData<'T>, TaskEither<'T>>
                (MoveNextMethodImpl<_>(fun sm ->
                    //-- RESUMABLE CODE START
                    __resumeAt sm.ResumptionPoint
                
                    let mutable __stack_exn: Exception = null

                    try
                        // printfn "Run BeforeInvoke Task.Status  --> %A" sm.Data.MethodBuilder.Task.Status
                        let __stack_code_fin = code.Invoke(&sm)
                        // printfn "Run Task.Status --> %A" sm.Data.MethodBuilder.Task.Status
                        // If the `sm.Data.MethodBuilder` has already been set somewhere else (like While/WhileDynamic), we shouldn't continue
                        if __stack_code_fin && not sm.Data.IsTaskCompleted then

                            // printfn "Run __stack_code_fin Data  --> %A" sm.Data.Result
                            sm.Data.MethodBuilder.SetResult(sm.Data.Result)
                    with
                    | exn -> __stack_exn <- exn
                    // Run SetException outside the stack unwind, see https://github.com/dotnet/roslyn/issues/26567
                    match __stack_exn with
                    | null -> ()
                    | exn -> sm.Data.MethodBuilder.SetException exn
                //-- RESUMABLE CODE END
                ))
                (SetStateMachineMethodImpl<_>(fun sm state -> sm.Data.MethodBuilder.SetStateMachine(state)))
                (AfterCode<_, _>(fun sm ->
                    sm.Data.MethodBuilder <- AsyncTaskEitherMethodBuilder<'T>.Create ()
                    sm.Data.MethodBuilder.Start(&sm)
                    sm.Data.MethodBuilder.Task))
        else
            TaskEitherBuilder.RunDynamic(code)
#endif
            
[<AutoOpen>]
module TaskEitherBuilder =
    #if !FABLE_COMPILER 
    let taskEither = TaskEitherBuilder()
    #else
    let taskEither = TaskEitherBuilderFableShim()
    #endif

#if !FABLE_COMPILER 

open Microsoft.FSharp.Control
open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators

[<AutoOpen>]
module TaskEitherCEExtensionsLowPriority =
    // Low priority extensions
    type TaskEitherBuilderBase with


        [<NoEagerConstraintApplication>]
        static member inline BindDynamic< ^TaskLike, 'TResult1, 'TResult2, ^Awaiter, 'TOverall, 'Error when ^TaskLike: (member GetAwaiter:
            unit -> ^Awaiter) and ^Awaiter :> ICriticalNotifyCompletion and ^Awaiter: (member get_IsCompleted:
            unit -> bool) and ^Awaiter: (member GetResult: unit -> Either<'TResult1>)>
            (
                sm: byref<_>,
                task: ^TaskLike,
                continuation: ('TResult1 -> TaskEitherCode<'TOverall, 'TResult2>)
            ) : bool =

            let mutable awaiter = (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) (task))

            let cont =
                (TaskEitherResumptionFunc<'TOverall>(fun sm ->
                    // printfn "ByndDynamic --> %A" sm.Data.Result

                    let result =
                        (^Awaiter: (member GetResult: unit -> Either<'TResult1>) (awaiter))

                    match result with
                    | Ok { Data = d; Warnings = w1 } -> 
                        sm.Data.AddWarnings w1
                        (continuation d).Invoke(&sm)
                    | Error e ->
                        sm.Data.Result <- Error e
                        true

                ))

            // shortcut to continue immediately
            if (^Awaiter: (member get_IsCompleted: unit -> bool) (awaiter)) then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                false

        [<NoEagerConstraintApplication>]
        member inline _.Bind< ^TaskLike, 'TResult1, 'TResult2, ^Awaiter, 'TOverall, 'Error when ^TaskLike: (member GetAwaiter:
            unit -> ^Awaiter) and ^Awaiter :> ICriticalNotifyCompletion and ^Awaiter: (member get_IsCompleted:
            unit -> bool) and ^Awaiter: (member GetResult: unit -> Either<'TResult1>)>
            (
                task: ^TaskLike,
                continuation: ('TResult1 -> TaskEitherCode<'TOverall, 'TResult2>)
            ) : TaskEitherCode<'TOverall, 'TResult2> =

            TaskEitherCode<'TOverall, _>(fun sm ->
                if __useResumableCode then
                    //-- RESUMABLE CODE START
                    // Get an awaiter from the awaitable
                    // printfn "Bynd --> %A" sm.Data.Result

                    let mutable awaiter = (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) (task))

                    let mutable __stack_fin = true

                    if not (^Awaiter: (member get_IsCompleted: unit -> bool) (awaiter)) then
                        // This will yield with __stack_yield_fin = false
                        // This will resume with __stack_yield_fin = true
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result =
                            (^Awaiter: (member GetResult: unit -> Either<'TResult1>) (awaiter))

                        match result with
                        | Ok { Data = d; Warnings = w1 } -> 
                            sm.Data.AddWarnings w1
                            (continuation d).Invoke(&sm)
                        | Error e ->
                            sm.Data.Result <- Error e
                            true
                    else
                        // printfn "Bind tasklike --> %A" sm.Data.MethodBuilder.Task.Status
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                        false
                else
                    TaskEitherBuilderBase.BindDynamic< ^TaskLike, 'TResult1, 'TResult2, ^Awaiter, 'TOverall, 'Error>(
                        &sm,
                        task,
                        continuation
                    )
            //-- RESUMABLE CODE END
            )

        [<NoEagerConstraintApplication>]
        member inline this.ReturnFrom< ^TaskLike, ^Awaiter, 'T when ^TaskLike: (member GetAwaiter:
            unit -> ^Awaiter) and ^Awaiter :> ICriticalNotifyCompletion and ^Awaiter: (member get_IsCompleted:
            unit -> bool) and ^Awaiter: (member GetResult: unit -> Either<'T>)>
            (task: ^TaskLike)
            : TaskEitherCode<_, _> =

            this.Bind(task, (fun v -> this.Return(v)))

        [<NoEagerConstraintApplication>]
        member inline this.Source< ^TaskLike, ^Awaiter, 'T, 'Error when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter) and ^Awaiter :> ICriticalNotifyCompletion and ^Awaiter: (member get_IsCompleted:
            unit -> bool) and ^Awaiter: (member GetResult: unit -> 'T)>
            (t: ^TaskLike)
            : TaskEither<'T> =

            task {
                let! r = t
                return Either.succeed r
            }

        member inline _.Using<'Resource, 'TOverall, 'T, 'Error when 'Resource :> IDisposable>
            (
                resource: 'Resource,
                body: 'Resource -> TaskEitherCode<'TOverall, 'T>
            ) =
            ResumableCode.Using(resource, body)

[<AutoOpen>]
module TaskEitherCEExtensionsHighPriority =
    // High priority extensions
    type TaskEitherBuilderBase with
        static member BindDynamic
            (
                sm: byref<_>,
                task: TaskEither<'TResult1>,
                continuation: ('TResult1 -> TaskEitherCode<'TOverall, 'TResult2>)
            ) : bool =
            let mutable awaiter = task.GetAwaiter()

            let cont =
                (TaskEitherResumptionFunc<'TOverall>(fun sm ->
                    // printfn "ByndDynamic --> %A" sm.Data.Result
                    let result = awaiter.GetResult()
                    match result with
                    | Ok { Data = d; Warnings = w1 } -> 
                        sm.Data.AddWarnings w1
                        (continuation d).Invoke(&sm)
                    | Error e ->
                        sm.Data.Result <- Error e
                        true))

            // shortcut to continue immediately
            if awaiter.IsCompleted then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                false

        member inline _.Bind
            (
                task: TaskEither<'TResult1>,
                continuation: ('TResult1 -> TaskEitherCode<'TOverall, 'TResult2>)
            ) : TaskEitherCode<'TOverall, 'TResult2> =

            TaskEitherCode<'TOverall, _>(fun sm ->
                if __useResumableCode then
                    //-- RESUMABLE CODE START
                    // Get an awaiter from the task
                    // printfn "Bynd--> %A" sm.Data.Result
                    let mutable awaiter = task.GetAwaiter()

                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        // This will yield with __stack_yield_fin = false
                        // This will resume with __stack_yield_fin = true
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = awaiter.GetResult()

                        match result with
                        | Ok { Data = d; Warnings = w1 } ->
                            sm.Data.AddWarnings w1
                            (continuation d).Invoke(&sm)
                        | Error e ->
                            sm.Data.Result <- Error e
                            true

                    else if sm.Data.MethodBuilder.Task.IsCompleted then
                        true
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                        false

                else
                    TaskEitherBuilderBase.BindDynamic(&sm, task, continuation)
            //-- RESUMABLE CODE END
            )

        member inline this.BindReturn(x: TaskEither<'T>, [<InlineIfLambda>] f) =
            this.Bind(x, (fun x -> this.Return(f x)))

        member inline _.MergeSources(t1: TaskEither<'T>, t2: TaskEither<'T1>) =  Plough.ControlFlow.TaskEither.zip t1 t2

        member inline this.ReturnFrom(task: TaskEither<'T>) : TaskEitherCode<_, _> =
            this.Bind(task, (fun v -> this.Return v))

        member inline _.Source(s: #seq<_>) = s

[<AutoOpen>]
   
module TaskEitherCEExtensionsMediumPriority =

    // Medium priority extensions
    type TaskEitherBuilderBase with
        member inline this.Source(t: Task<'T>) : TaskEither<'T> = t |> Task.map Either.succeed

        member inline this.Source(t: Task) : TaskEither<unit> = task {
            do! t
            return Either.succeed ()
        }
    #if NETSTANDARD2_1
        member inline this.Source(t: ValueTask<'T>) : TaskEither<'T> = t |> Task.mapV Ok

        member inline this.Source(t: ValueTask) : TaskEither<unit> = task {
            do! t
            return  Either.succeed ()
        }
    #endif
        member inline this.Source(computation: Async<'T>) : TaskEither<'T> =
            computation |> Async.StartAsTask |> Task.map Either.succeed 
#endif
    open System.Collections.Generic
    [<AbstractClass>]
    type TaskEither() =
        static member inline collect (source : (unit -> TaskEither<'a>) seq) : TaskEither<'a seq> =
            taskEither {
                let results = List<'a>()
                
                for item in source do
                    let! result = item ()
                    results.Add(result)
                
                return upcast results
            }
            
        static member inline collectMany (source : (unit -> TaskEither<'a seq>) seq) : TaskEither<'a seq> =
            taskEither {
                let results = List<'a>()
                
                for item in source do
                    let! result = item ()
                    results.AddRange(result)
                
                return upcast results
            }
        
        static member inline collect (source : (unit -> TaskEither<'a>) list) : TaskEither<'a list> =
            taskEither {
                let mutable results = List.empty<'a>
                
                for item in source do
                    let! result = item ()
                    results <- results @ [result]
                
                return results
            }
            
        static member inline collectMany (source : (unit -> TaskEither<'a list>) list) : TaskEither<'a list> =
            taskEither {
                let mutable results = List.empty<'a>
                
                for item in source do
                    let! result = item ()
                    results <- results @ result
                
                return results
            }

        static member inline collect (source : (unit -> TaskEither<'a>) []) : TaskEither<'a []> = 
            taskEither {
                let results = Array.init source.Length (fun _ -> Unchecked.defaultof<'a>)
                
                for i in 0 .. source.Length - 1 do
                    let! result = source.[i] ()
                    results.[i] <- result
                
                return results
            }
            
        static member inline collectMany (source : (unit -> TaskEither<'a []>) []) : TaskEither<'a []> = 
            taskEither {
                let results = List()
                
                for item in source do
                    let! result = item ()
                    results.AddRange(result)
                
                return results.ToArray()
            }
