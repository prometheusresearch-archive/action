module Typed = Workflow.Typed
module Untyped = Workflow.Untyped
module Map = Common.StringMap

module Make (Universe : Abstract.UNIVERSE) = struct

  module QueryTyper = QueryTyper.Make(Universe)

  type error = [ `WorkflowTypeError of string | `QueryTypeError of string ]
  type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

  let workflowTypeError err = Run.error (`WorkflowTypeError err)

  let typeWorkflow ~univ w =
    let open Run.Syntax in
    let rec aux ~scope ~parent w =
      match w with
      | Untyped.Root -> return (Typed.Root, Query.Typed.void, Map.empty)
      | Untyped.Label name ->
        begin match Map.get scope name with
        | Some tq -> return (Typed.Label name, tq, scope)
        | None -> workflowTypeError {j|referencing unknown workflow "$name"|j}
        end
      | Untyped.Render {query; label} ->
        let render = Typed.Render {query; label} in
        let%bind tq = QueryTyper.growQuery ~univ ~base:parent query in
        let scope = match label with
        | Some label -> Map.set scope label tq
        | None -> scope
        in
        return (render, tq, scope)
      | Untyped.AndThen (first, next) ->
        let%bind first, parent, scope = aux ~scope ~parent first in
        let%bind next, _ =
          let f (next, parent) w =
            let%bind w, _, _ = aux ~scope ~parent w in
            return (w::next, parent)
          in
          Run.List.foldLeft ~f ~init:([], parent) next
        in
        return (Typed.AndThen (first, List.rev next), parent, scope)
    in
    let scope = Map.empty in
    let%bind tw, _, _ = aux ~scope ~parent:Query.Typed.void w in
    return tw

end
