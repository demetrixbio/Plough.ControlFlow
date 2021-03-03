module Plough.DynamicQuery.Operator

let (&&&) x y =
    match x, y with
    | Simple simple, Complex { Condition = And; Predicates = predicates }
    | Complex { Condition = And; Predicates = predicates }, Simple simple ->
        { Condition = And; Predicates = Simple simple :: predicates } |> Complex
    | Complex { Condition = And; Predicates = p1 }, Complex { Condition = And; Predicates = p2 } ->
        { Condition = And; Predicates = p1 @ p2 } |> Complex
    | _ ->
        { Condition = And
          Predicates = [ x; y ] } |> Complex

let (|||) x y =
    match x, y with
    | Simple simple, Complex { Condition = Or; Predicates = predicates }
    | Complex { Condition = Or; Predicates = predicates }, Simple simple ->
        { Condition = Or; Predicates = Simple simple :: predicates } |> Complex
    | Complex { Condition = Or; Predicates = p1 }, Complex { Condition = Or; Predicates = p2 } ->
        { Condition = Or; Predicates = p1 @ p2 } |> Complex
    | _ ->
        { Condition = Or
          Predicates = [ x; y ] } |> Complex