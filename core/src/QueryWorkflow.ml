module QueryMonoid = struct
  type t = Query.Untyped.t

  let empty = Query.Untyped.Syntax.void
  let append a b = Query.Untyped.Syntax.grow b a
  let show = Query.Untyped.show
end

module Workflow = Workflow.Make(QueryMonoid)
module Syntax = Workflow.Syntax

(**
 * Workflow.
 *)
type t = Workflow.t
type workflow = Workflow.t

(**
 * Workflow state.
 *
 * This type represents workflow state, a position within a workflow AST and a
 * list of arguments to apply to the current value at the workflow state.
 *)
type state = {
  workflow : Workflow.t;
  db : JSONDatabase.t;
  ui : Value.UI.t;
  pos : Workflow.Pos.t;
  posWithArgs : Workflow.Pos.t;
  prev : state option;
}

let runQuery ~db query =
  let open Run.Syntax in
  let univ = JSONDatabase.univ db in
  let%bind query = JSONDatabase.QueryTyper.typeQuery ~univ query in
  JSONDatabase.query ~db query

let positionToState ~workflow ~db ~prev pos =
  let open Run.Syntax in
  let query = Workflow.Pos.value pos in
  let%bind value = runQuery ~db query in
  begin match Value.classify value with
  | Value.UI ui -> return (Some {
      workflow;
      db;
      ui;
      pos; posWithArgs = pos;
      prev;
    })
  | Value.Null -> return None
  | _ -> Workflow.workflowError "expected UI"
  end

let rec firstOfPositions ~workflow ~db ~prev =
  let open Run.Syntax in
  function
  | [] -> return None
  | pos::rest ->
    begin match%bind positionToState ~workflow ~prev ~db pos with
    | Some state -> return (Some state)
    | None -> firstOfPositions ~db ~workflow ~prev rest
    end

let run ~db workflow =
  let open Run.Syntax in
  let%bind pos = Workflow.Pos.run ~label:"main" workflow in
  let%bind next = Workflow.Pos.next pos in
  match%bind firstOfPositions ~db ~workflow ~prev:None next with
  | None -> Workflow.workflowError "no available actions"
  | Some state -> return state

(**
 * Get the query which corresponds to the workflow [state].
 *)
let query state =
  Workflow.Pos.value state.posWithArgs

let id state =
  state.pos |> Workflow.Pos.value |> Query.Untyped.show

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
      |> Workflow.Pos.value
      |> Query.Untyped.Syntax.growArgs args
    in
    Workflow.Pos.replaceValue value state.pos
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
  let%bind next = Workflow.Pos.next state.posWithArgs in
  let f next pos =
    match%bind positionToState ~prev:(Some state) ~workflow:state.workflow ~db:state.db pos with
    | Some state -> return (state::next)
    | None -> return next
  in
  Run.List.foldLeft ~f ~init:[] next

let around state =
  let open Run.Syntax in
  let%bind around =
    match state.prev with
    | None ->
      let%bind pos = Workflow.Pos.run ~label:"main" state.workflow in
      let%bind next = Workflow.Pos.next pos in
      let f next pos =
        match%bind positionToState ~prev:None ~workflow:state.workflow ~db:state.db pos with
        | Some state -> return (state::next)
        | None -> return next
      in
      Run.List.foldLeft ~f ~init:[] next
    | Some state ->
      next state
  in
  return (List.rev around)

let rebuild state =
  let open Run.Syntax in
  let rec aux ~prev =
    function
    | [] -> return prev
    | state::rest ->
      let%bind next =
        positionToState
          ~workflow:state.workflow
          ~db:state.db
          ~prev
          state.posWithArgs
      in
      begin match next with
      | Some next -> aux ~prev:(Some next) rest
      | None -> return prev
      end
  in
  match%bind aux ~prev:None (breadcrumb state) with
  | Some state -> return state
  | None -> run ~db:state.db state.workflow

let mutate ~mutation ~value state =
  let open Run.Syntax in
  let%bind () = Mutation.execute ~mutation value in
  let%bind state = rebuild state in
  return state
