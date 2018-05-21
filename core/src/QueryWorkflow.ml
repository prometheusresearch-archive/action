module QueryMonoid = struct
  type t = Query.Untyped.t

  let empty = Query.Untyped.Syntax.void
  let append a b = Query.Untyped.Syntax.grow b a
  let show = Query.Untyped.show
end

module Lang = WorkflowLang.Make(QueryMonoid)
module Syntax = Lang.Syntax

(**
 * Workflow.
 *)
type workflow = Lang.t

(**
 * Workflow state.
 *
 * This type represents workflow state, a position within a workflow AST and a
 * list of arguments to apply to the current value at the workflow state.
 *)
type state = {
  workflow : Lang.t;
  db : JSONDatabase.t;
  ui : Value.UI.t;
  pos : Lang.Pos.t;
  posWithArgs : Lang.Pos.t;
  prev : state option;
}

let runQuery ~db query =
  let open Run.Syntax in
  let univ = JSONDatabase.univ db in
  let%bind query = JSONDatabase.QueryTyper.typeQuery ~univ query in
  JSONDatabase.query ~db query

let positionToState ~workflow ~db pos =
  let open Run.Syntax in
  let query = Lang.Pos.value pos in
  let%bind value = runQuery ~db query in
  begin match Value.classify value with
  | Value.UI ui -> return (Some {
      workflow;
      db;
      ui;
      pos; posWithArgs = pos;
      prev = None
    })
  | Value.Null -> return None
  | _ -> Lang.workflowError "expected UI"
  end

let rec firstOfPositions ~workflow ~db =
  let open Run.Syntax in
  function
  | [] -> return None
  | pos::rest ->
    begin match%bind positionToState ~workflow ~db pos with
    | Some state -> return (Some state)
    | None -> firstOfPositions ~db ~workflow rest
    end

let run ~db workflow =
  let open Run.Syntax in
  let%bind pos = Lang.Pos.run ~label:"main" workflow in
  let%bind next = Lang.Pos.next pos in
  match%bind firstOfPositions ~db ~workflow next with
  | None -> Lang.workflowError "no available actions"
  | Some state -> return state

(**
 * Get the query which corresponds to the workflow [state].
 *)
let query state =
  Lang.Pos.value state.posWithArgs

(**
 * Render [state] into UI.
 *)
let ui state =
  state.ui

(**
 * Execute query [q] against [state] context.
 *)
let execute q state =
  let open Run.Syntax in
  let univ = JSONDatabase.univ state.db in
  let%bind query =
    let posQuery = query state in
    let query = Query.Untyped.Syntax.grow q posQuery in
    let%bind query = JSONDatabase.QueryTyper.typeQuery ~univ query in
    return query
  in
  JSONDatabase.query ~db:state.db query

let replaceArgs args state =
  let posWithArgs =
    let value =
      state.pos
      |> Lang.Pos.value
      |> Query.Untyped.Syntax.growArgs args
    in
    Lang.Pos.replaceValue value state.pos
  in
  {state with posWithArgs}

(**
 * Get a list of all states in the workflow.
 *)
let breadcrumb state =
  let rec aux breadcrumb state =
    let breadcrumb = state::breadcrumb in
    match state.prev with
    | Some state -> aux breadcrumb state
    | None -> breadcrumb
  in aux [] state

(**
 * Get a list of next possible states.
 *)
let next state =
  let open Run.Syntax in
  let%bind next = Lang.Pos.next state.posWithArgs in
  let f next pos =
    match%bind positionToState ~workflow:state.workflow ~db:state.db pos with
    | Some state -> return (state::next)
    | None -> return next
  in
  Run.List.foldLeft ~f ~init:[] next

let around state =
  let open Run.Syntax in
  match state.prev with
  | None ->
    let%bind pos = Lang.Pos.run ~label:"main" state.workflow in
    let%bind next = Lang.Pos.next pos in
    let f next pos =
      match%bind positionToState ~workflow:state.workflow ~db:state.db pos with
      | Some state -> return (state::next)
      | None -> return next
    in
    Run.List.foldLeft ~f ~init:[] next
  | Some state ->
    next state
