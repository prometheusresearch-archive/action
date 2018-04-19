(**
 * Untyped workflow AST.
 *)
module Untyped : sig

  type t =

    (** Root node *)
    | Root

    (** Render a query *)
    | Render of render

    (** Branching, define what's possible next moves for the workflow *)
    | AndThen of (t * t list)

    (** Refer to the previously defined render node *)
    | Label of string

  and render = {
    query : Query.Untyped.t;
    label : string option;
  }

  (**
   * Convenience for build workflow AST programmatically.
   *)
  module Syntax : sig
    val root : t
    val render : ?label : string -> Query.Untyped.t -> t
    val andThen : t list -> t -> t
    val label : string -> t
  end
end

(**
 * Typed workflow AST.
 *)
module Typed : sig

  type t =

    | Root

    (** Render concrete query to a screen *)
    | Render of Untyped.render

    (** Define how to transition from one screen to another screen *)
    | AndThen of t * t list

    (** Refer to the previously defined render node *)
    | Label of string

end
