(**
 * JS API
 *)
open Core

module Q = Core.UntypedQuery.Syntax
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

  let currentValue ~screenArgs ~execute here =
    let open Result.Syntax in
    let%bind value = execute here in
    let idQuery =
      let argQuery = TypedQuery.unsafeLookupArg ~name:"id" screenArgs in
      TypedQuery.unsafeChain here argQuery
    in
    let%bind id = execute idQuery in

    match Value.classify id, Value.classify value with
    | Value.String id, Value.Array value ->
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
    | Value.Number id, Value.Array value ->
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
    | _ -> return Value.null
  in

  let resolveData ~screenArgs:_ ~args:_ ~execute here =
    execute here
  in
  let resolveValue ~screenArgs ~args:_ ~execute here =
    currentValue ~screenArgs ~execute here
  in
  let resolveTitle ~screenArgs ~args:_ ~execute here =
    let open Result.Syntax in
    let%bind value = currentValue ~screenArgs ~execute here in
    let%bind titleBase =
      let q = TypedQuery.unsafeLookupArg ~name:"title" screenArgs in
      let q = TypedQuery.unsafeChain here q in
      execute q
    in
    let titleSuffix =
      Value.get ~name:"id" value
      |> Option.alt (Value.get ~name:"title" value)
      |> Option.alt (Value.get ~name:"name" value)
      |> Option.getWithDefault Value.null
    in
    match Value.classify titleBase, Value.classify titleSuffix with
    | Value.String base, Value.String title -> return (Value.string {j|$base ($title)|j})
    | Value.String base, Value.Null -> return (Value.string {j|$base|j})
    | _ -> return (Value.string {j|Pick|j})
  in
  Screen.Syntax.(screen
    ~inputCard:Card.Many
    ~navigate:"value"
    ~args:[
      arg "id" ~default:Q.null (one number);
      arg "title" ~default:(Q.string "Pick") (one string);
    ]
    (fun entity -> [
      has ~resolve:resolveTitle "title" (one string);
      has ~resolve:resolveValue "value" (opt entity);
      has ~resolve:resolveData "data" (many entity);
    ])
  )

let viewScreen =
  let resolveData ~screenArgs:_ ~args:_ ~execute here =
    let open Result.Syntax in
    let%bind value = execute here in
    return value
  in
  let resolveValue ~screenArgs:_ ~args:_ ~execute here =
    let open Result.Syntax in
    let%bind value = execute here in
    return value
  in
  let resolveTitle ~screenArgs ~args:_ ~execute here =
    let open Result.Syntax in
    let%bind value = execute here in
    let%bind titleBase =
      let q = TypedQuery.unsafeLookupArg ~name:"title" screenArgs in
      let q = TypedQuery.unsafeChain here q in
      execute q
    in
    let titleSuffix =
      Value.get ~name:"id" value
      |> Option.alt (Value.get ~name:"title" value)
      |> Option.alt (Value.get ~name:"name" value)
    in
    match titleBase, titleSuffix with
    | base, Some title -> Result.Ok (Value.string {j|$base ($title)|j})
    | base, None -> Result.Ok (Value.string {j|$base|j})
  in
  Screen.Syntax.(screen
    ~inputCard:Card.One
    ~navigate:"value"
    ~args:[
      arg "title" ~default:(Q.string "View") (one string);
    ]
    (fun entity -> [
      has ~resolve:resolveTitle "title" (one string);
      has ~resolve:resolveValue "value" (one entity);
      has ~resolve:resolveData "data" (one entity);
    ])
  )

let barChartScreen =
  let resolveData ~screenArgs:_ ~args:_ ~execute here =
    let open Result.Syntax in
    let%bind value = execute here in
    return value
  in
  let resolveValue ~screenArgs:_ ~args:_ ~execute here =
    let open Result.Syntax in
    let%bind value = execute here in
    return value
  in
  let resolveTitle ~screenArgs ~args:_ ~execute here =
    let open Result.Syntax in
    let%bind value = execute here in
    let%bind titleBase =
      let q = TypedQuery.unsafeLookupArg ~name:"title" screenArgs in
      let q = TypedQuery.unsafeChain here q in
      execute q
    in
    let titleSuffix =
      Value.get ~name:"id" value
      |> Option.alt (Value.get ~name:"title" value)
      |> Option.alt (Value.get ~name:"name" value)
    in
    match titleBase, titleSuffix with
    | base, Some title -> Result.Ok (Value.string {j|$base ($title)|j})
    | base, None -> Result.Ok (Value.string {j|$base|j})
  in
  Screen.Syntax.(screen
    ~inputCard:Card.Many
    ~navigate:"value"
    ~args:[
      arg "title" ~default:(Q.string "View") (one string);
    ]
    (fun entity -> [
      has ~resolve:resolveTitle "title" (one string);
      has ~resolve:resolveValue "value" (one entity);
      has ~resolve:resolveData "data" (one entity);
    ])
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

let workflow =
  let open UntypedWorkflow.Syntax in
  let open UntypedQuery.Syntax in

  let pickRegion = render (
    here
    |> nav "region"
    |> screen ~args:[arg "title" (string "Regions")] "pick"
  ) in

  let pickNation = render (
    here
    |> nav "nation"
    |> screen ~args:[arg "title" (string "Nations")] "pick"
  ) in

  let pickCustomer = render (
    here
    |> nav "customer"
    |> screen ~args:[arg "title" (string "Customers")] "pick"
  ) in

  let view title = render (
    here
    |> screen ~args:[arg "title" (string title)]"view"
  ) in

  let pickViewCustomer = pickCustomer |> andThen [
    view "View Customer"
  ] in

  let pickViewNation = pickNation |> andThen [
    view "View Nation" |> andThen [ pickViewCustomer ];
    pickViewCustomer
  ] in

  let pickViewRegion = pickRegion |> andThen [
    view "View Region" |> andThen [ pickViewNation ];
    pickViewNation
  ] in

  pickViewRegion

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
    let args = StringMap.(empty |> fun m -> set m "id" (UntypedQuery.Syntax.number id)) in
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

let uiArgs ui =
  let args = Value.UI.args ui in
  let value = Value.UI.value ui in
  let baseQuery = Value.UI.query ui in
  let f args name argQuery =
    let query = TypedQuery.unsafeChain baseQuery argQuery in
    let value = unwrapResult (JSONDatabase.execute ~value ~db query) in
    Js.Dict.set args name value;
    args
  in StringMap.reduce args (Js.Dict.empty ()) f

