namespace Plough.DynamicQuery

#if !FABLE_COMPILER

open System
open Dapper
open System.Text
open System.Collections.Generic
open System.Data

type SqlRequest =
    { Sql : string
      Parameters : obj }
    
type private Clause =
    { Sql : string
      Parameters : DynamicParameters
      IsInclusive : bool }

type private Clauses =
    { Joiner : string
      Prefix : string
      Postfix : string
      Items : List<Clause> }

// Pagination
type PageInfo(?index: int64, ?size: int64, ?orderColumns: (string * SortDirection) list, ?calculateTotalCount: bool) =
    member __.Index = defaultArg index 0L
    member __.Size = defaultArg size 50L
    member __.OrderColumns = defaultArg orderColumns []
    member __.CalculateTotalCount = defaultArg calculateTotalCount true

module internal Helpers =
    open System.Text.RegularExpressions

    let placeholderRegex = Regex(@"\/\*\*.+?\*\*\/", RegexOptions.Compiled ||| RegexOptions.Multiline)

    let dictToParams (param : IDictionary<string, obj> option) =
        let p = DynamicParameters()
        for KeyValue(k, v) in param |> Option.defaultValue (dict []) do
            match v with
            | :? DateTime ->
                // It's murky but per https://gitter.im/npgsql/npgsql?at=56b76d4c939ffd5d15f64970
                // dapper seems to convert DateTime to timestamptz which is what we want
                // You don't seem to be able to send in NpgsqlDbType.TimestampTz explicitly
                p.Add(k,v,Nullable DbType.DateTime)
            | :? Nullable<DateTime> as d ->
                let value = if d.HasValue then Nullable (d.Value.ToLocalTime()) else Nullable ()
                p.Add(k, value, Nullable DbType.DateTime)
            | _ ->
                p.Add(k, v)
        p

type SqlBuilder private (template : string, parameters : DynamicParameters) =
    let mutable dataSeq = 0
    let mutable seq = 1
    let mutable rawSql = ""
    let mutable data : Dictionary<string, Clauses> = Dictionary()
    
    new (?template : string, ?param : IDictionary<string, obj>) =
        SqlBuilder(template |> Option.defaultValue "/**--parameters**/",
                   param |> Helpers.dictToParams)

    member __.Build () =
        if dataSeq <> seq then
            rawSql <-
                let state = template  |> StringBuilder
                for clause in data do
                    let temp = sprintf "/**%s**/" clause.Key
                    for item in clause.Value.Items do parameters.AddDynamicParams(item.Parameters)
                    let joined =
                        clause.Value.Items
                        |> Seq.map (fun s -> s.Sql, s.IsInclusive)
                        |> Array.ofSeq
                        |> Array.partition snd
                        |> fun (i, n) ->
                            i |> Array.map fst,
                            n |> Array.map fst
                        |> function
                            | [| |], [| |] -> ""
                            | [| |], nonInclusive -> nonInclusive |> String.concat clause.Value.Joiner
                            | inclusive, nonInclusive ->
                                let inclusiveFold = inclusive |> String.concat " OR " |> sprintf "(%s)"
                                nonInclusive
                                |> Array.append [| inclusiveFold |]
                                |> String.concat clause.Value.Joiner
                        
                    state.Replace(temp, clause.Value.Prefix + joined + clause.Value.Postfix) |> ignore

                // replace all that is left with empty
                Helpers.placeholderRegex.Replace(state.ToString(), "");
            dataSeq <- seq
        
        { Sql = rawSql
          Parameters = parameters }
        
    member x.Compose (?sql : string, ?param : IDictionary<string, obj>) =
        x.Build() |> ignore
        let p = param |> Helpers.dictToParams 
        if not <| isNull parameters then p.AddDynamicParams(parameters)
        let template = sprintf "%s\n%s" rawSql (sql |> Option.defaultValue "/**--parameters**/")
        SqlBuilder(template, p)
        
    member private x.AddClause(name, sql, parameters : IDictionary<string, obj> option, joiner, ?prefix, ?postfix, ?isInclusive) =
        let clauses = 
            match data.TryGetValue name with
            | false, _ ->
                let result =
                    { Joiner = joiner
                      Prefix = prefix |> Option.defaultValue ""
                      Postfix = postfix |> Option.defaultValue ""
                      Items = List() }
                data.[name] <- result
                result
            | true, result -> result
        { Sql = sql
          Parameters = parameters |> Helpers.dictToParams
          IsInclusive = isInclusive |> Option.defaultValue false } |> clauses.Items.Add
        seq <- seq + 1
        x

    member x.Parametrized(param) = x.AddClause("--parameters", "", Some param, "")
    member x.Intersect(sql, param) = x.AddClause("intersect", sql, param, "\nINTERSECT\n ", "\n ", "\n", false)
    member x.InnerJoin(sql, param) = x.AddClause("innerjoin", sql, param, "\nINNER JOIN ", "\nINNER JOIN ", "\n", false)
    member x.LeftJoin(sql, param) = x.AddClause("leftjoin", sql, param, "\nLEFT JOIN ", "\nLEFT JOIN ", "\n", false)
    member x.Join(sql, param) = x.AddClause("join", sql, param, "\nJOIN ", "\nJOIN ", "\n", false)
    member x.Where(sql, param) = x.AddClause("where", sql, param, " AND ", "WHERE ", "\n", false) 
    member x.OrWhere(sql, param) = x.AddClause("where", sql, param, " OR ", "WHERE ", "\n", true)
    member x.OrderBy(sql, param) = x.AddClause("orderby", sql, param, " , ", "ORDER BY ", "\n", false)
    member x.Select(sql, param) = x.AddClause("select", sql, param, " , ", "", "\n", false)
    member x.GroupBy(sql, param) = x.AddClause("groupby", sql, param, " , ", "\nGROUP BY ", "\n", false)
    member x.Having(sql, param) = x.AddClause("having", sql, param, "\nAND", "HAVING ", "\n", false)
    member x.Limit(n : int64) =
        let limit = Guid.NewGuid().ToString("N");
        x.AddClause("limit", sprintf "@%s" limit, Some (dict [ limit, box n ]), " , ", "LIMIT ", "\n", false)
    member x.Offset(n : int64) =
        let offset = Guid.NewGuid().ToString("N");
        x.AddClause("offset", sprintf "@%s" offset, Some (dict [ offset, box n ]), " , ", "OFFSET ", "\n", false)

[<RequireQualifiedAccess>]
module SqlBuilder =

    let init sql param = SqlBuilder(sql, param)
    let parametrized param (builder : SqlBuilder) = builder.Parametrized(param)
    let intersect sql param (builder : SqlBuilder) = builder.Intersect(sql, param)
    let innerJoin sql param (builder : SqlBuilder) = builder.InnerJoin(sql, param)
    let leftJoin sql param (builder : SqlBuilder) = builder.LeftJoin(sql, param)
    let join sql param (builder : SqlBuilder) = builder.Join(sql, param)
    let where sql param (builder : SqlBuilder) = builder.Where(sql, param)
    let orWhere sql param (builder : SqlBuilder) = builder.OrWhere(sql, param)
    let orderBy sql param (builder : SqlBuilder) = builder.OrderBy(sql, param)
    let select sql param (builder : SqlBuilder) = builder.Select(sql, param)
    let groupBy sql param (builder : SqlBuilder) = builder.GroupBy(sql, param)
    let having sql param (builder : SqlBuilder) = builder.Having(sql, param)
    let offset (n : int64) (builder : SqlBuilder) = builder.Offset(n)
    let limit (n : int64) (builder : SqlBuilder) = builder.Limit(n)
    let build (builder : SqlBuilder) = builder.Build()

    let private runPagedPrivate<'OutRecord>
                    (connection : IDbConnection) (page: PageInfo)
                    (transform: (IDictionary<string, obj> -> 'OutRecord) option)
                    (sql: SqlRequest) =
        
        async {
            let parameters = DynamicParameters(sql.Parameters)
            parameters.Add("offset", page.Index * page.Size)
            parameters.Add("limit", page.Size)

            let countSql = sprintf "SELECT COUNT(1) FROM (%s) AS t;" sql.Sql
            
            let orderBy =
                match page.OrderColumns with
                | [] -> failwithf "Missing order column: %s" sql.Sql
                | orderColumns ->
                    orderColumns
                    |> Seq.map (fun (col, sortDirection) ->
                        let direction = match sortDirection with | Ascending -> "ASC" | Descending -> "DESC"
                        sprintf "%s %s" col direction)
                    |> String.concat ", "
                    |> sprintf "ORDER BY %s"
                    
            let itemsSql = sprintf "%s %s OFFSET @offset LIMIT @limit;" sql.Sql orderBy

            let! itemsEnum =
                match transform with
                | Some transform ->
                    async {
                        let! items = connection.QueryAsync(itemsSql, parameters) |> Async.AwaitTask
                        return items |> Seq.map (fun x -> transform(x :?> _))
                    }
                | None ->
                    connection.QueryAsync<'OutRecord>(itemsSql, parameters) |> Async.AwaitTask

            let items = List.ofSeq itemsEnum
            let actualSize = List.length items |> int64

            let! count =
                async {
                    if not page.CalculateTotalCount then
                        return None
                    elif actualSize < page.Size then
                        return (page.Index * page.Size) + actualSize |> int64 |> Some
                    else
                        let! count = connection.QuerySingleAsync<int64>(countSql, parameters) |> Async.AwaitTask
                        return Some count
                }

            return { Items = items
                     PageSize = page.Size
                     PageIndex = page.Index
                     TotalItemCount = count }
        }


    let runPagedWithTransform (connection : IDbConnection) (page: PageInfo)
                              (transform: IDictionary<string, obj> -> 'OutRecord)
                              (sql: SqlRequest) =
        runPagedPrivate connection page (Some transform) sql

    let runPaged<'OutRecord> (connection : IDbConnection) (page: PageInfo) (sql: SqlRequest) =
        runPagedPrivate<'OutRecord> connection page None sql

#endif