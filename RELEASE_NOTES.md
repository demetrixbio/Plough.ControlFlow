## New in 0.1.0 (Released 2021/02/21)
* Release initial ControlFlow library of Plough

## New in 0.2.0 (Released 2021/02/25)
* Add compatibility for Fable

## New in 0.2.1 (Released 2021/02/25)
* Fix packaging files for Fable

## New in 0.2.2 (Released 2021/02/26)
* Fix packaging files for Fable

## New in 0.2.3 (Released 2021/03/01)
* Lazy collect implementation for Either/TaskEither

## New in 0.2.4 (Released 2021/03/01)
* Lazy collect implementation for Either/TaskEither

## New in 0.2.5 (Released 2021/03/01)
* Fable compatible collect implementation for Either/TaskEither

## New in 0.3.0 (Released 2021/03/03)
* Extra extensions added for task interop
* DynamicQuery added

## New in 0.3.1 (Released 2021/03/03)
* DynamicQuery operators require opening a module to not trigger global override

## New in 0.3.2 (Released 2021/03/03)
* Single project publish

## New in 0.3.3 (Released 2021/03/04)
* Reverted to multi project publish, fixed Fable tasks

## New in 0.3.4 (Released 2021/03/04)
* Fix Fable tasks

## New in 0.3.5-6 (Released 2021/03/04)
* Single Plough library for ControlFlow and DynamicQuery for Fable compatibility

## New in 0.4.0 (Released 2021/03/09)
* Separate Plough.ControlFlow library

## New in 0.4.1 (Released 2021/03/09)
* FSharp.Core version relaxed to 4.7.2

## New in 0.4.2 (Released 2021/03/13)
* Publish Plough.ControlFlow without conflicting dependencies

## New in 0.4.3 (Released 2021/03/24)
* Fix TaskEither.onError 

## New in 0.5.0 (Released 2021/05/07)
* Add Task.collect and Task.collectMany

## New in 0.5.1 (Released 2021/06/21)
* Fix Fable tasks for Task.collect and Task.collectMany

## New in 0.5.2 (Released 2021/11/09)
* Fix TaskEither.For/While and Either.For/While to quit processing cycle on inner error
* Fix TaskEither.onError / Either.onError - exception error processed only once, rethrow original error

## New in 1.0.0 (Released 2021/12/23)
* F# 6 with native task support

## New in 1.0.1 (Released 2021/12/23)
* Brought back Ply task CE due to transaction scope problem with native task CE - https://github.com/dotnet/fsharp/issues/12556

## New in 1.0.2 (Released 2022/09/06)
* Include original exception as an inner exception in TaskEither.toTask. #6

## New in 1.1.0 (Released 2022/09/26)
* Replace TaskEither.onError with TaskEither.catch.
* Add TaskEither.teeErrorAsync.