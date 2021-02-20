namespace Plough.ControlFlow

[<RequireQualifiedAccess>]
module Option =

  let traverseEither f opt =
    match opt with
    | None -> Either.succeed None
    | Some v -> f v |> Either.map Some

  let sequenceEither opt = 
    traverseEither id opt
