(**
 * JS API
 *)
open Core

module Q = Core.Query.Syntax
module T = Core.Type.Syntax
module S = Core.Screen.Syntax

module WorkflowInterpreter = WorkflowInterpreter(JSONDatabase)

module JsResult = struct
  type 'v t

  let ok value = Obj.magic [%bs.obj {_type = "Ok"; value;}]
  let error error = Obj.magic [%bs.obj {_type = "Error"; error;}]

  let ofResult = function
    | Result.Ok v -> ok v
    | Result.Error err -> error err

end

type ui = Value.UI.t
type state = WorkflowInterpreter.t
type renderableState = < state : state; ui : ui Js.Nullable.t > Js.t
type query = TypedQuery.t

let showQuery = TypedQuery.show

let unwrapResult v =
  match v with
  | Result.Ok v -> v
  | Result.Error err -> Js.Exn.raiseError err

let id state =
  state |> WorkflowInterpreter.uiQuery |> unwrapResult |> TypedQuery.show

let pickScreen =

  Screen.Syntax.(screen
    ~inputCard:Card.Many
    ~args:[
      arg "id" ~default:Q.null (one number);
      arg "title" ~default:(Q.string "Pick") (one string);
    ]
    Q.(
      let parent = name "parent" in
      let title = name "title" in
      let value = name "value" in
      let id = name "id" in
      void
      |> select [
        field ~alias:"data" parent;
        field ~alias:"value" value;
        field ~alias:"title" title;
      ]
      |> where [
        arg "value" (parent |> locate id);
      ]
    )
  )

let viewScreen =
  Screen.Syntax.(screen
    ~inputCard:Card.One
    ~args:[
      arg "title" ~default:(Q.string "View") (one string);
    ]
    Q.(
      void
      |> select [
        field ~alias:"data" (name "parent");
        field ~alias:"value" (name "parent");
        field ~alias:"title" (name "title");
      ]
      |> where [
      ]
    )
  )

let barChartScreen =
  Screen.Syntax.(screen
    ~inputCard:Card.Many
    ~args:[
      arg "title" ~default:(Q.string "View") (one string);
    ]
    Q.(
      void
      |> select [
        field ~alias:"data" (name "parent");
        field ~alias:"value" (name "parent");
        field ~alias:"title" (name "title");
      ]
    )
  )

let univ =

  let regionNationCustomer = Type.Syntax.(entity "customer" [
    hasOne "name" string;
    hasOne "comment" string;
    hasOne "phone" string;
    hasOne "acctbal" string;
  ]) in

  let regionNation = Type.Syntax.(entity "nation" [
    hasOne "name" string;
    hasOne "comment" string;
    hasMany "customer" regionNationCustomer;
  ]) in

  let region = Type.Syntax.(entity "region" [
    hasOne "name" string;
    hasOne "comment" string;
    hasMany "nation" regionNation;
  ]) in

  Universe.(
    empty
    |> hasMany "region" region
    |> hasScreen "pick" pickScreen
    |> hasScreen "view" viewScreen
    |> hasScreen "barChart" barChartScreen
  )

external data : Js.Json.t = "data" [@@bs.module "./data.js"]

let db = JSONDatabase.ofJson ~univ data

let toJS state =
  JsResult.ofResult (
    let open Result.Syntax in
    let%bind state, ui = state in
    Result.Ok [%bs.obj { state; ui = Js.Nullable.from_opt ui }]
  )

let breadcrumbs state =
  state |> WorkflowInterpreter.breadcrumbs |> Array.of_list

let next state =
  match WorkflowInterpreter.next state with
  | Result.Ok next -> Array.of_list next
  | Result.Error err -> Js.Exn.raiseError err

let render state =
  toJS (WorkflowInterpreter.render state)

let pickValue id state =
  toJS (
    let open Result.Syntax in
    let args = StringMap.(empty |> fun m -> set m "id" (Q.number id)) in
    let%bind state = WorkflowInterpreter.setArgs ~args state in
    let%bind state = WorkflowInterpreter.step state in
    WorkflowInterpreter.render state
  )

let executeQuery q =
  let res =
    let open Result.Syntax in
    let%bind q = q in
    let%bind data = JSONDatabase.execute ~db q in
    return data
  in match res with
  | Result.Ok data -> data
  | Result.Error err -> Js.Exn.raiseError err

let getData state =
  executeQuery (WorkflowInterpreter.dataQuery state)

let getTitle state =
  executeQuery (WorkflowInterpreter.titleQuery state)

let parse s =
  let module N = Js.Nullable in
  let makeError err = [%bs.obj {error = N.return err; ui = N.null; data = N.null;}] in
  let r =
    let open Result.Syntax in
    let makeData data =
      return [%bs.obj {error = N.null; ui = N.null; data = N.return data;}]
    in
    let makeError err =
      return (makeError err)
    in
    let makeWorkflow w =
      let state =
        let%bind w = WorkflowTyper.typeWorkflow ~univ w in
        WorkflowInterpreter.boot ~db w
      in
      return [%bs.obj {error = N.null; data = N.null; ui = N.return (toJS state)}]
    in
    let makeUi ui =
      let w = UntypedWorkflow.Syntax.render ui in
      makeWorkflow w
    in
    let filebuf = Lexing.from_string s in
    try
      match Parser.start Lexer.read filebuf with
      | Core.ParseResult.Workflow w ->
        makeWorkflow w
      | Core.ParseResult.Query q ->
        let%bind tq = QueryTyper.typeQuery ~univ q in
        let%bind value = JSONDatabase.execute ~db tq in
        match Value.classify value with
        | Value.UI _ -> makeUi q
        | _ -> makeData value
    with
    | Lexer.Error msg ->
      makeError msg
    | Parser.Error ->
      makeError "Syntax Error"
  in match r with
  | Result.Ok r -> r
  | Result.Error err -> makeError err

let uiName = Value.UI.name
