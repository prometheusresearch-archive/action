(**
 * NOTE: It is a good idea to keep this as minimal as possinle and only contain
 * code which lifts OCaml to JS idiom (e.g. result values' errors to raised
 * exceptions).
 *)

module Json = Js.Json

type state = QueryWorkflow.state
type workflow = QueryWorkflow.workflow
type args = Json.t
type ui = Value.UI.t
type db = JSONDatabase.t

let runExn comp = match Run.toResult comp with
  | Js.Result.Ok v -> v
  | Js.Result.Error (`WorkflowError err) -> Js.Exn.raiseError err
  | Js.Result.Error (`DatabaseError err) -> Js.Exn.raiseError err
  | Js.Result.Error (`QueryTypeError err) -> Js.Exn.raiseError err

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
  runExn (QueryWorkflow.next state)
