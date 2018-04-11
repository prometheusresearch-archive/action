(**
 * Database mutations.
 *)

module Untyped : sig
  (**
  * A mutation is a set of ops, one per field.
  *)
  type t

  module Syntax : sig

    type op

    (** Update value with the query result *)
    val update : name : string -> Query.Untyped.t -> op
    (** Update entity *)
    val updateEntity : name : string -> op list -> op
    (** Create new entity *)
    val createEntity : name : string -> op list -> op

    (** Create mutation given a list of ops *)
    val mutation : op list -> t
  end
end

module Typed : sig

  (**
  * A mutation is a set of ops, one per field.
  *)
  type t = op Common.StringMap.t

  (**
  * Operation on a single field.
  *)
  and op =
    (** Update value with the query result *)
    | Update of Query.Untyped.t
    (** Update entity *)
    | UpdateEntity of t
    (** Create new entity *)
    | CreateEntity of t
end

module Typer : sig

  type error = [ `QueryTypeError of string ]

  type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

  val typeMutation : univ : Universe.t -> query : Query.Typed.t -> Untyped.t -> (Typed.t, 'err) comp
end
