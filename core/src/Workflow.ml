module Result = Common.Result

(**
 * Monadic structure on top queries which represent transition between screens.
 *)
module Syntax (Q : sig type t end) = struct

  type q = Q.t
  type t =
    (** Render concrete query to a screen *)
    | Render of q
    (** Define how to transition from one screen to another screen *)
    | Next of (t * t list)

end

module Untyped = struct
  include Syntax(Query.Untyped)

  module Syntax = struct
    let render q = Render q
    let andThen path w = Next (w, path)
  end
end

module Typed = struct
  include Syntax(struct

    type t = Query.Untyped.t

  end)
end

module Typer = struct

  type error = [ `WorkflowTypeError of string | `QueryTypeError of string ]
  type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

  let workflowTypeError err = Run.error (`WorkflowTypeError err)

  let liftResult = function
    | Result.Ok v -> Run.return v
    | Result.Error err -> Run.error (`WorkflowTypeError err)

  let typeWorkflow ~univ w =
    let open Run.Syntax in
    let rec aux ~parent w =
      match w with
      | Untyped.Render q ->
        let%bind tq = QueryTyper.growQuery ~univ ~base:parent q in
        return (Typed.Render q, tq)
      | Untyped.Next (first, next) ->
        let%bind first, parent = aux ~parent first in
        let%bind next, _ =
          let f (next, parent) w =
            let%bind w, _ = aux ~parent w in
            return (w::next, parent)
          in
          Run.List.foldLeft ~f ~init:([], parent) next
        in
        return (Typed.Next (first, List.rev next), parent)
    in
    let%bind tw, _ = aux ~parent:Query.Typed.void w in
    return tw

end

