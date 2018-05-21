(**
 * NOTE: It is a good idea to keep this as minimal as possinle and only contain
 * code which lifts OCaml to JS idiom (e.g. result values' errors to raised
 * exceptions).
 *)

module Json = Js.Json

type state = QueryWorkflow.state
type workflow = QueryWorkflow.workflow
type args = Json.t
type db = JSONDatabase.t
type ui = Value.UI.t
type value = Value.t
type query = string

let runExn comp = match Run.toResult comp with
  | Js.Result.Ok v -> v
  | Js.Result.Error (`WorkflowError err) -> Js.Exn.raiseError err
  | Js.Result.Error (`DatabaseError err) -> Js.Exn.raiseError err
  | Js.Result.Error (`QueryTypeError err) -> Js.Exn.raiseError err
  | Js.Result.Error (`ParseError err) -> Js.Exn.raiseError err

let db = Config.db

let run db workflow =
  runExn (QueryWorkflow.run ~db workflow)

let replaceArgs args state =
  let parseArgs args =
    let f args (name, value) =
      let value =
        match Json.classify value with
        | Json.JSONTrue -> Query.Untyped.Syntax.bool true
        | Json.JSONFalse -> Query.Untyped.Syntax.bool false
        | Json.JSONNumber v -> Query.Untyped.Syntax.number v
        | Json.JSONString v -> Query.Untyped.Syntax.string v
        | Json.JSONNull -> Query.Untyped.Syntax.null
        | Json.JSONArray _
        | Json.JSONObject _ ->
          Js.Exn.raiseError "invalid argument type"
      in
      (Query.Untyped.Syntax.arg name value)::args
    in
    Belt.Array.reduce (Js.Dict.entries args) [] f
  in
  match Json.classify args with
  | Json.JSONObject args ->
    let args = parseArgs args in
    QueryWorkflow.replaceArgs args state
  | _ -> Js.Exn.raiseError "expected arguments to be an object"

let ui state =
  QueryWorkflow.ui state

let next state =
  state
  |> QueryWorkflow.next
  |> runExn
  |> Array.of_list

let around state =
  state
  |> QueryWorkflow.around
  |> runExn
  |> Array.of_list

let breadcrumb state =
  state
  |> QueryWorkflow.breadcrumb
  |> Array.of_list

let parse s =
  let open Run.Syntax in
  let filebuf = Lexing.from_string s in
  match Parser.start Lexer.read filebuf with
  | value -> return value
  | exception Lexer.Error msg ->
    error (`ParseError msg)
  | exception Parser.Error ->
    let msg = {j|syntax error while parsing $s|j} in
    error (`ParseError msg)

let parseQueryResult s =
  let open Run.Syntax in
  match%bind parse s with
  | ParserResult.Query q -> return q
  | ParserResult.Workflow _ -> error (`ParseError "expected query")

let parseWorkflowResult s =
  let open Run.Syntax in
  match%bind parse s with
  | ParserResult.Query _ -> error (`ParseError "expected workflow")
  | ParserResult.Workflow w -> return w

let parseWorkflow s =
  runExn (parseWorkflowResult s)

let query query state =
  runExn (
    let open Run.Syntax in
    let%bind query = parseQueryResult query in
    let%bind value = QueryWorkflow.execute query state in
    return value
  )
