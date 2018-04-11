module Result = Common.Result

module Make (Db : Abstract.DATABASE) = struct

  type t = frame * Value.UI.t option

  and frame = {
    db : Db.t;
    query : Query.Typed.t;
    workflow : Workflow.Typed.t;
    position: position;
    prev: t option;
    args : Query.Untyped.args;
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

  let liftResult = function
    | Result.Ok v -> Run.return v
    | Result.Error err -> Run.error (`RunWorkflowError err)

  let uiQuery (frame, _) =
    let open Run.Syntax in
    match frame.workflow with
    | Workflow.Typed.Render q ->
      let%bind q = QueryTyper.growQuery ~univ:(Db.univ frame.db) ~base:frame.query q in
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
    } in frame, ui

  let findRender startState =
    let open Run.Syntax in

    let rec aux (frame, _ as state) =
      let {workflow; db; query; _} = frame in
      match workflow with
      | Workflow.Typed.Next (first, _next) ->
        let state = make ?prev:frame.prev ~position:(First state) ~query ~db first in
        aux state
      | Workflow.Typed.Render _ ->
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
    let state = make ~position:Root ~db workflow in
    let%bind state, ui = render state in
    return (state, ui)

  let next (_, _ as currentState) =
    let open Run.Syntax in

    let rec aux query (frame, _ as state) =
      let {workflow; position; db;} = frame in
      match workflow, position with
      | Workflow.Typed.Render _, First parent -> aux query parent
      | Workflow.Typed.Render _, Next _  -> return []
      | Workflow.Typed.Render _, Root -> return []
      | Workflow.Typed.Next (_first, []), First parent -> aux query parent
      | Workflow.Typed.Next (_first, next), _ ->
        let f w =
          let state = make ~prev:currentState ~query ~position:(Next state) ~db w in
          findRender state
        in
        let%bind next = Run.List.map ~f next in
        return (Common.Option.List.filterNone next)
    in

    let%bind q = uiQuery currentState in
    let%bind r = aux q currentState in
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
    | Workflow.Typed.Render (ctyp, Query.Untyped.Screen (p, c)) ->
      let univ = Db.univ frame.db in
      let%bind screen = liftResult (Universe.lookupScreenResult c.screenName univ) in
      let%bind args = QueryTyper.checkArgsPartial ~argTyps:screen.args args in
      let c = {
        c with Query.Untyped.
        screenArgs = Query.Untyped.updateArgs ~update:args c.screenArgs
      } in
      let workflow = Workflow.Typed.Render (ctyp, Query.Untyped.Screen (p, c)) in
      return { frame with args; workflow; }
    | _ -> runWorkflowError {j|Arguments can only be updated at the workflow node with a rendered screen|j}
    in
    return (frame, ui)

end

