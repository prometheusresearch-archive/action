open Core

(**
 * Monadic structure on top queries which represent transition between screens.
 *)
module Syntax (Q : sig
  type t
  val show : t -> string
end) = struct

  type q = Q.t
  type t =
    (** Render concrete query to a screen *)
    | Render of q
    (** Define how to transition from one screen to another screen *)
    | Next of (t * t list)

  let rec show v =
    match v with
    | Render q ->
        let q = Q.show q in {j|render($q)|j}
    | Next (w, next) ->
      let w = show w
      and next = next |> List.map show |> String.concat ", "
      in {j|$w { $next }|j}

end

module Untyped = struct
  include Syntax(Query)

  module Syntax = struct
    let render q = Render q
    let andThen path w = Next (w, path)
  end
end

module Typed = struct
  include Syntax(struct

    type t = Query.t

    let show w =
      Query.show w

  end)
end

module Typer = struct

  type error = [ `WorkflowTypeError of string ]
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
        let%bind tq = liftResult (QueryTyper.growQuery ~univ ~base:parent q) in
        begin match tq with
        | _, TypedQuery.Screen _ ->
          return (Typed.Render q, tq)
        | q ->
          let q = TypedQuery.show q in
          let msg = {j|workflow can only be defined on screen syntax but got $q|j} in
          workflowTypeError msg
        end
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
    let%bind tw, _ = aux ~parent:TypedQuery.void w in
    return tw

end

