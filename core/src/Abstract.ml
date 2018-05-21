(**
 * This module contains abstract interface declarations for the action.
 *)

module type MONOID = sig
  type t

  val empty : t
  val append : t -> t -> t
  val show : t -> string
end


(**
 * Universe provides an API to query for database schema and configured screens.
 *)
module type UNIVERSE = sig

  (** Universe *)
  type t

  (** Entities are named objects in a universe *)
  module Entity : sig

    type t

    val name : t -> string
    val fields : t -> Query.Type.field list

  end

  (** A list of fields available in a universe *)
  val fields : t -> Query.Type.field list

  (** Lookup entity by name *)
  val getEntity : string -> t -> Entity.t option

  (** Lookup screen by name *)
  val getScreen : string -> t -> Screen.t option

end

(**
 * Database
 *)
module type DATABASE = sig

  module Universe : UNIVERSE

  (** Abstract type which represents a database instance/handle *)
  type t

  (** Errors in which database computations could result *)
  type error = [ `DatabaseError of string | `QueryTypeError of string ]

  (** Computations performed with the database *)
  type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

  (**
   * Universe associated with the database.
   *)
  val univ : t -> Universe.t

  (**
   * Root value of the database.
   *)
  val root : t -> Value.t

  (**
   * Execute typed query.
   *)
  val query :
    ?value : Value.t
    -> db : t
    -> Query.Typed.t
    -> (Value.t, 'err) comp

end
