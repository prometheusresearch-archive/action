(**
 * Database mutations.
 *)

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


module Syntax : sig

  type opSyntax

  val update : name : string -> Query.Untyped.t -> opSyntax
  val updateEntity : name : string -> opSyntax list -> opSyntax
  val createEntity : name : string -> opSyntax list -> opSyntax

  val mutation : opSyntax list -> t
end
