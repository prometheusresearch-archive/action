module Map = Common.StringMap
module Result = Common.Result
module W = Workflow.Typed

module Make (Db : Abstract.DATABASE) = struct

  module QueryTyper = QueryTyper.Make(Db.Universe)

  type t = frame * Value.UI.t option

  and frame = {
    db : Db.t;
    query : Query.Typed.t;
    workflow : W.t;
    position: position;
    prev: t option;
    args : Query.Untyped.args;
    scope : t Map.t;
  }

  and position =
    | Root
    | First of t
    | Next of t

  type error = [
    `RunWorkflowError of string
  | `DatabaseError of string
  | `QueryTypeError of string
  ]
  type ('v, 'err) comp = ('v,  [> error ] as 'err) Run.t

  let runWorkflowError err = Run.error (`RunWorkflowError err)

  let getScreen name univ =
    let open Run.Syntax in
    match Db.Universe.getScreen name univ with
    | Some screen -> return screen
    | None -> runWorkflowError {j|Unknown screen "$name"|j}

  let uiQuery (frame, _) =
    let open Run.Syntax in
    match frame.workflow with
    | W.Render {query; _} ->
      let%bind q =
        QueryTyper.growQuery
          ~univ:(Db.univ frame.db)
          ~base:frame.query
          query
      in
      return q
    | _ -> runWorkflowError "no query"

  let rec breadcrumbs (frame, _ as state) =
    match frame.prev with
    | Some prev -> state::(breadcrumbs prev)
    | None -> [state]

  let rec show (frame, _ as state) =
    let query = match Run.toResult (uiQuery state) with
    | Result.Ok query -> Query.Typed.show query
    | Result.Error _ -> "EMPTY"
    in
    match frame.prev with
    | None -> {j|$query <- ROOT|j}
    | Some prev -> let prev = show prev in {j|$query <- $prev|j}

  let make
    ?prev
    ?ui
    ?(args=Common.StringMap.empty)
    ?(query=Query.Typed.void)
    ~scope
    ~position
    ~db
    workflow
    =
    let frame = {
      query;
      db;
      workflow;
      position;
      prev;
      args;
      scope;
    } in frame, ui

  let findRender startState =
    let open Run.Syntax in

    let rec aux (frame, ui as state) =
      let {workflow; db; query; _} = frame in
      match workflow with
      | W.Root -> return None
      | W.Label name ->
        begin match Map.get frame.scope name with
        | Some state -> aux state
        | None -> runWorkflowError {j|unknown binding "$name"|j}
        end
      | W.AndThen (first, _next) ->
        let state = make ?prev:frame.prev ~scope:frame.scope ~position:(First state) ~query ~db first in
        aux state
      | W.Render {label; _} ->
        let state = match label with
        | Some label ->
          let scope = Map.set frame.scope label state in
          {frame with scope}, ui
        | None -> state
        in
        let%bind q = uiQuery state in
        let%bind ui = Db.query ~db:frame.db q in
        begin match Value.classify ui with
        | Value.Null -> return None
        | Value.UI _ -> return (Some state)
        | _ -> runWorkflowError "not an ui"
        end
    in

    aux startState

  let render state =
    let open Run.Syntax in

    let render (frame, _ as state) =
      let%bind q = uiQuery state in
      let%bind res = Db.query ~db:frame.db q in
      match Value.classify res with
      | Value.UI ui ->
        let ui = Value.UI.setArgs ~args:frame.args ui in
        return ((frame, Some ui), Some ui)
      | Value.Null ->
        return ((frame, None), None)
      | _ -> runWorkflowError "expected UI, got data"
    in

    match%bind findRender state with
    | Some state -> render state
    | None -> return (state, None)

  let boot ~db workflow =
    let open Run.Syntax in
    let state = make ~position:Root ~db ~scope:Map.empty workflow in
    let%bind state, ui = render state in
    return (state, ui)

  let next (frame, _ as currentState) =
    let open Run.Syntax in

    let rec aux ~scope query (frame, _ as state) =
      let {workflow; position; db;} = frame in
      match workflow, position with
      | W.Root, Root -> return []
      | W.Root, First parent -> aux ~scope query parent
      | W.Root, Next _ -> return []
      | W.Label label, _ ->
        let%bind state = match Map.get scope label with
        | Some state -> return state
        | None -> runWorkflowError {j|unreachable label "$label"|j}
        in aux ~scope query state
      | W.Render {Workflow.Untyped. label;_}, First parent ->
        let scope = match label with
        | Some label -> Map.set scope label state
        | None -> scope
        in
        aux ~scope query parent
      | W.Render _, Next _  -> return []
      | W.Render _, Root -> return []
      | W.AndThen (_first, []), First parent ->
        aux ~scope query parent
      | W.AndThen (_first, next), _ ->
        let f w =
          let state = make ~prev:currentState ~query ~scope:scope ~position:(Next state) ~db w in
          findRender state
        in
        let%bind next = Run.List.map ~f next in
        return (Common.Option.List.filterNone next)
    in

    let%bind q = uiQuery currentState in
    Js.log2 "NEXT" (Map.toArray frame.scope);
    let%bind r =
      aux
        ~scope:frame.scope
        q
        currentState
    in
    return r

  let step state =
    let open Run.Syntax in
    let%bind next = next state in
    match next with
    | [] -> return state
    | state::_ -> return state

  let setArgs ~args (frame, ui) =
    let open Run.Syntax in
    let%bind frame = match frame.workflow with
    | W.Render {query = ctyp, Query.Untyped.Screen (p, c); label} ->
      let univ = Db.univ frame.db in
      let%bind screen = getScreen c.screenName univ in
      let%bind args = QueryTyper.checkArgsPartial ~argTyps:screen.args args in
      let c = {
        c with Query.Untyped.
        screenArgs = Query.Untyped.updateArgs ~update:args c.screenArgs
      } in
      let workflow = W.Render {query = ctyp, Query.Untyped.Screen (p, c); label} in
      return { frame with args; workflow; }
    | _ -> runWorkflowError {j|Arguments can only be updated at the workflow node with a rendered screen|j}
    in
    return (frame, ui)

end

