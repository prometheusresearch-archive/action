(**
 * JS API
 *)

module Q = Query.Untyped.Syntax
module T = Query.Type.Syntax
module S = Screen.Syntax
module Result = Common.Result

module WorkflowInterpreter = RunWorkflow.Make(JSONDatabase)

let runToResult v = match Run.toResult v with
  | Result.Ok v -> Result.Ok v
  | Result.Error (`DatabaseError err) ->
    let msg = {j|DatabaseError: $err|j} in
    Result.Error msg
  | Result.Error (`RunWorkflowError err) ->
    let msg = {j|WorkflowError: $err|j} in
    Result.Error msg
  | Result.Error (`WorkflowTypeError err) ->
    let msg = {j|WorkflowTypeError: $err|j} in
    Result.Error msg
  | Result.Error (`QueryTypeError err) ->
    let msg = {j|QueryTypeError: $err|j} in
    Result.Error msg
  | Result.Error (`ParseError err) ->
    let msg = {j|ParseError: $err|j} in
    Result.Error msg

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
type query = Query.Typed.t

let showQuery = Query.Typed.show

let unwrapResult v =
  match v with
  | Result.Ok v -> v
  | Result.Error err -> Js.Exn.raiseError err

let id state =
  state |> WorkflowInterpreter.uiQuery |> runToResult |> unwrapResult |> Query.Typed.show

let pickScreen =

  Screen.Syntax.(screen
    ~inputCard:Query.Card.Many
    ~args:[
      arg "id" ~default:Q.null (one number);
      arg "title" ~default:(Q.string "Pick") (one string);
      arg "fields" ~default:(Q.here) (one string);
    ]
    Q.(
      let parent = name "parent" in
      let title = name "title" in
      let fields = name "fields" in
      let id = name "id" in
      let base = void |> select [
          field ~alias:"data" parent;
          field ~alias:"value" (parent |> locate id);
        ]
      in
      void
      |> select [
        field ~alias:"data" parent;
        field ~alias:"dataForUI" (parent |> grow fields);
        field ~alias:"value" (base |> nav "value");
        field ~alias:"title" (base |> grow title);
      ]
    )
  )

let viewScreen =
  Screen.Syntax.(screen
    ~inputCard:Query.Card.One
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

let editScreen =
  Screen.Syntax.(screen
    ~inputCard:Query.Card.One
    ~args:[
      arg "title" ~default:(Q.string "Edit") (one string);
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

let barChartScreen =
  Screen.Syntax.(screen
    ~inputCard:Query.Card.Many
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

  let rec customer = lazy Query.Type.Syntax.(entity "customer" (fun _ -> [
    hasOne "id" string;
    hasOne "name" string;
    hasOne "comment" string;
    hasOne "phone" string;
    hasOne "acctbal" string;
  ]))

  and nation = lazy Query.Type.Syntax.(entity "nation" (fun _ -> [
    hasOne "id" string;
    hasOne "name" string;
    hasOne "comment" string;
    hasMany "customer" (Lazy.force customer);
    hasOne "region" (Lazy.force region);
  ]))

  and region = lazy Query.Type.Syntax.(entity "region" (fun _ -> [
    hasOne "id" string;
    hasOne "name" string;
    hasOne "comment" string;
    hasMany "nation" (Lazy.force nation);
  ]))

  in

  Universe.(
    empty

    |> hasMany "region" (Lazy.force region)
    |> hasMany "nation" (Lazy.force nation)
    |> hasMany "customer" (Lazy.force customer)

    |> hasScreen "pick" pickScreen
    |> hasScreen "view" viewScreen
    |> hasScreen "edit" editScreen
    |> hasScreen "barChart" barChartScreen
  )

external data : Js.Json.t = "data" [@@bs.module "./data.js"]

let db = JSONDatabase.ofJson ~univ data

let toJS state =
  JsResult.ofResult (
    let open Result.Syntax in
    let%bind state, ui = state in
    Result.Ok [%bs.obj { state; ui = Js.Nullable.fromOption ui }]
  )

let breadcrumbs state =
  state |> WorkflowInterpreter.breadcrumbs |> Array.of_list

let next state =
  match runToResult (WorkflowInterpreter.next state) with
  | Result.Ok next -> Array.of_list next
  | Result.Error err -> Js.Exn.raiseError err

let render state =
  toJS (runToResult (WorkflowInterpreter.render state))

let pickValue id state =
  let id = match Js.Json.classify id with
  | Js.Json.JSONNumber id -> Q.number id
  | Js.Json.JSONString id -> Q.string id
  | _ -> Js.Exn.raiseError "invalid id for pickValue"
  in
  Js.log2 "pickValue.id" id;
  toJS (
    let open Result.Syntax in
    let args = Common.StringMap.(empty |> fun m -> set m "id" id) in
    Js.log2 "pickValue" (Query.Untyped.showArgs args);
    let%bind state = runToResult (WorkflowInterpreter.setArgs ~args state) in
    let%bind state = runToResult (WorkflowInterpreter.step state) in
    runToResult (WorkflowInterpreter.render state)
  )

let executeQuery q =
  let res =
    let open Run.Syntax in
    let%bind q = q in
    let%bind data = JSONDatabase.query ~db q in
    return data
  in match runToResult res with
  | Result.Ok data -> data
  | Result.Error err -> Js.Exn.raiseError err

let parseQuery q =
  let open Run.Syntax in
  let filebuf = Lexing.from_string q in
  try
    match Parser.start Lexer.read filebuf with
    | ParserResult.Workflow _ ->
      error (`ParseError "expected query")
    | ParserResult.Query q ->
      return q
  with
  | Lexer.Error msg ->
    error (`ParseError msg)
  | Parser.Error ->
    error (`ParseError "syntax error")


let query state q =
  executeQuery (
    let open Run.Syntax in
    let%bind q = parseQuery q in
    let%bind base = WorkflowInterpreter.uiQuery state in
    let%bind q = QueryTyper.growQuery ~univ ~base q in
    Js.log3 "QUERY" (WorkflowInterpreter.show state) (Query.Typed.show q);
    return q
  )

let parse s =
  let module N = Js.Nullable in
  let makeError err = [%bs.obj {error = N.return err; ui = N.null; data = N.null;}] in
  let r =
    let makeData data =
      Run.return [%bs.obj {error = N.null; ui = N.null; data = N.return data;}]
    in
    let makeError err =
      Run.return (makeError err)
    in
    let makeWorkflow w =
      let state =
        runToResult (
          let open Run.Syntax in
          let%bind w = Workflow.Typer.typeWorkflow ~univ w in
          WorkflowInterpreter.boot ~db w
        )
      in
      Run.return [%bs.obj {error = N.null; data = N.null; ui = N.return (toJS state)}]
    in
    let makeUi ui =
      let w = Workflow.Untyped.Syntax.render ui in
      makeWorkflow w
    in
    let open Run.Syntax in
    let filebuf = Lexing.from_string s in
    try
      match Parser.start Lexer.read filebuf with
      | ParserResult.Workflow w ->
        makeWorkflow w
      | ParserResult.Query q ->
        let%bind tq = QueryTyper.typeQuery ~univ q in
        let%bind value = JSONDatabase.query ~db tq in
        match Value.classify value with
        | Value.UI _ -> makeUi q
        | _ -> makeData value
    with
    | Lexer.Error msg ->
      makeError msg
    | Parser.Error ->
      makeError "Syntax Error"
  in match runToResult r with
  | Result.Ok r -> r
  | Result.Error err -> makeError err

let uiName = Value.UI.name
