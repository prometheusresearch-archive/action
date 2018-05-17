module QueryMonoid = struct
  type t = Query.Untyped.t

  let empty = Query.Untyped.Syntax.void
  let append = Query.Untyped.Syntax.grow
  let show = Query.Untyped.show
end

module Lang = WorkflowLang.Make(QueryMonoid)

let workflowError msg =
  Run.error (`WorkflowError msg)

let render ~db pos =
  let open Run.Syntax in
  let univ = JSONDatabase.univ db in
  let%bind query =
    let query = Lang.Pos.value pos in
    let%bind query = JSONDatabase.QueryTyper.typeQuery ~univ query in
    return query
  in
  let%bind value = JSONDatabase.query ~db query in
  match Value.classify value with
  | Value.UI ui -> return ui
  | _ -> workflowError "expected UI"

let query ~db ~pos query =
  let open Run.Syntax in
  let univ = JSONDatabase.univ db in
  let%bind query =
    let posQuery = Lang.Pos.value pos in
    let query = Query.Untyped.Syntax.grow posQuery query in
    let%bind query = JSONDatabase.QueryTyper.typeQuery ~univ query in
    return query
  in
  JSONDatabase.query ~db query

let breadcrumb pos =
  let rec aux breadcrumb pos =
    let breadcrumb = pos::breadcrumb in
    match Lang.Pos.prev pos with
    | Some pos -> aux breadcrumb pos
    | None -> breadcrumb
  in aux [] pos
