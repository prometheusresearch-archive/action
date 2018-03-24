(**
 * JS API
 *)
open Core

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

let uiName = Value.UI.name
let uiArgs ui =
  match Value.UI.args ui with
  | None -> Js.Dict.empty ()
  | Some args ->
    let f args {Arg. name; value} =
      let value = match value with
      | Arg.String v -> Js.Json.string v
      | Arg.StringList v -> v |> Array.of_list |> Js.Json.stringArray
      | Arg.Number v -> Js.Json.number v
      | Arg.Bool v -> Js.Json.boolean (Js.Boolean.to_js_boolean v)
      in
      Js.Dict.set args name value;
      args
    in Belt.List.reduce args (Js.Dict.empty ()) f

let pickScreen =
  let resolveData ~screenArgs:_ ~args:_ value =
    let open Result.Syntax in
    return value
  in
  let resolveValue ~screenArgs ~args:_ value =
    let id = Arg.findValueFromArgList ~name:"id" screenArgs in
    match id, Value.classify value with
    | None, _ ->
      Result.Ok Value.null
    | Some (Arg.Number id), Value.Array value ->
      Result.Ok (
        if Array.length value = 0
        then Value.null
        else
          let f value = match Value.classify value with
          | Value.Object obj -> begin match Js.Dict.get obj "id" with
            | Some v -> (Obj.magic v) = id
            | None -> false
            end
          | _ -> false
          in
          Value.ofOption (Js.Array.find f value))
    | _ -> Result.Error "invalid invocation"
  in
  let resolveTitle ~screenArgs ~args:_ value =
    let open Result.Syntax in
    let%bind value = resolveValue ~screenArgs ~args:[] value in
    let title =
      Value.get ~name:"id" value
      |> Option.alt (Value.get ~name:"title" value)
      |> Option.alt (Value.get ~name:"name" value)
    in
    match title with
    | Some title -> Result.Ok (Value.string {j|Pick ($title)|j})
    | None -> Result.Ok (Value.string {j|Pick|j})
  in
  Screen.Syntax.(screen
    ~inputCard:Card.Many
    ~navigate:"value"
    (fun entity -> [
      has ~resolve:resolveTitle "title" string;
      has ~resolve:resolveValue ~card:Card.Opt "value" entity;
      has ~resolve:resolveData ~card:Card.Many "data" entity;
    ])
  )

let viewScreen =
  let resolveData ~screenArgs:_ ~args:_ value =
    let open Result.Syntax in
    return value
  in
  let resolveValue ~screenArgs:_ ~args:_ value =
    Result.Ok value
  in
  let resolveTitle ~screenArgs ~args:_ value =
    let open Result.Syntax in
    let%bind value = resolveValue ~screenArgs ~args:[] value in
    let title =
      Value.get ~name:"id" value
      |> Option.alt (Value.get ~name:"title" value)
      |> Option.alt (Value.get ~name:"name" value)
    in
    match title with
    | Some title -> Result.Ok (Value.string {j|View ($title)|j})
    | None -> Result.Ok (Value.string {j|View|j})
  in
  Screen.Syntax.(screen
    ~inputCard:Card.One
    ~navigate:"value"
    (fun entity -> [
      has ~resolve:resolveTitle "title" string;
      has ~resolve:resolveValue ~card:Card.Opt "value" entity;
      has ~resolve:resolveData ~card:Card.Opt "data" entity;
    ])
  )

let univ =

  let site = Type.Syntax.(entity "site" [
    hasOne "title" string;
  ]) in

  let individual = Type.Syntax.(entity "individual" [
    hasOne "name" string;
    hasOpt "site" site;
  ]) in

  Universe.(
    empty
    |> hasMany "individual" individual
    |> hasScreen "pick" pickScreen
    |> hasScreen "view" viewScreen
  )

let db = JSONDatabase.ofString ~univ {|
  {
    "individual": [
      {
        "id": 1,
        "name": "Andrey Popp",
        "site": {
          "title": "RexDB Site"
        }
      },
      {
        "id": 2,
        "name": "Oleksiy Golovko",
        "site": {
          "title": "Portal Site"
        }
      },
      {
        "id": 3,
        "name": "Clark Evans",
        "site": {
          "title": "Portal Site"
        }
      },
      {
        "id": 4,
        "name": "This individual has no site",
        "site": null
      }
    ]
  }
|}


let workflow =
  let open UntypedWorkflow.Syntax in
  let open UntypedQuery.Syntax in

  let pickIndividual = render (
    here
    |> nav "individual"
    |> screen ~args:[Arg.stringList "fields" ["id"; "name"]] "pick"
  ) in

  let view = render (
    here
    |> screen "view"
  ) in

  let viewSite = render (
    here
    |> nav "site"
    |> screen "view"
  ) in

  let viewName = render (
    here
    |> nav "name"
    |> screen ~args:[Arg.string "title" "Name"] "view"
  ) in

  pickIndividual |> andThen [
    view |> andThen [
      viewName;
      viewSite;
    ];
    viewSite;
  ]

let toJS state =
  JsResult.ofResult (
    let open Result.Syntax in
    let%bind state, ui = state in
    Result.Ok [%bs.obj { state; ui = Js.Nullable.from_opt ui }]
  )

let start =
  let open Result.Syntax in
  toJS (
    let%bind w = WorkflowTyper.typeWorkflow ~univ workflow in
    WorkflowInterpreter.boot ~db w
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
    let args = Some [Arg.number "id" id] in
    let state = WorkflowInterpreter.setArgs ~args state in
    let%bind state = WorkflowInterpreter.step state in
    WorkflowInterpreter.render state
  )

let executeQuery q =
  let res =
    let open Result.Syntax in
    let%bind q = q in
    let%bind data = JSONDatabase.execute db q in
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
        let%bind value = JSONDatabase.execute db tq in
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
