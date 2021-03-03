namespace Plough.DynamicQuery

[<RequireQualifiedAccess>]
module QueryInterpreter =

    open System
    open System.Collections.Generic

    let mapInt = function
        | Operation.Int.Equals value -> "=", value |> box
        | Operation.Int.GreaterThen value -> ">", value |> box
        | Operation.Int.LessThen value -> "<", value |> box
        | Operation.Int.GreaterThenOrEquals value -> ">=", value |> box
        | Operation.Int.LessThenOrEquals value -> "<=", value |> box
        
    let mapDecimal = function
        | Operation.Decimal.Equals value -> "=", value |> box
        | Operation.Decimal.GreaterThen value -> ">", value |> box
        | Operation.Decimal.LessThen value -> "<", value |> box
        | Operation.Decimal.GreaterThenOrEquals value -> ">=", value |> box
        | Operation.Decimal.LessThenOrEquals value -> "<=", value |> box
      
    let mapTimestamp = function
        | Operation.Timestamp.Equals value -> "=", value |> box
        | Operation.Timestamp.GreaterThen value -> ">", value |> box
        | Operation.Timestamp.LessThen value -> "<", value |> box
        | Operation.Timestamp.GreaterThenOrEquals value -> ">=", value |> box
        | Operation.Timestamp.LessThenOrEquals value -> "<=", value |> box
      
    let mapString = function
        | Operation.String.Equals value -> "=", value |> box
        | Operation.String.Like value -> "LIKE", value |> box
     
    let mapBool = function
        | Operation.Bool.Equals value -> "=", value |> box
        
    let rec translate state mapSimple = function
        | Simple predicate -> mapSimple state predicate
        | Complex { Predicates = [] } | Empty -> "1=1"
        | Complex { Predicates = [ predicate ] } -> translate state mapSimple predicate
        | Complex expr ->
            let condition = match expr.Condition with | And -> " AND " | Or -> " OR "
            expr.Predicates
            |> List.map (fun predicate ->
                        match predicate with
                        | Simple _ -> translate state mapSimple predicate
                        | Complex _ | Empty -> translate state mapSimple predicate |> sprintf "(%s)")
            |> String.concat condition
    
    #if !FABLE_COMPILER
         
    type State =
        { Template : string
          InnerJoins : Dictionary<string, string list>
          LeftJoins : Dictionary<string, string list>
          Parameters : Dictionary<string, obj> }
        
    let toSql state column (op, p) =
        let valueRef = Guid.NewGuid().ToString("N");
        state.Parameters.Add(valueRef, p)
        sprintf "%s %s @%s" column op valueRef
            
    let mapToSql query state mapSimple =
        let predicates = translate state mapSimple query |> sprintf "%s"
        let mutable acc = SqlBuilder.init state.Template state.Parameters |> SqlBuilder.where predicates None
        for join in state.InnerJoins |> Seq.collect (fun s -> s.Value) do
            acc <- acc |> SqlBuilder.innerJoin join None
        for join in state.LeftJoins |> Seq.collect (fun s -> s.Value) do
            acc <- acc |> SqlBuilder.leftJoin join None
        acc
        
    #endif