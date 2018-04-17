(**
 * A data structure which implements nested scopes.
 *)

(** Scope which maps names to values 'v *)
type 'v t

(** Scoped name is just a string *)
type name = string

(** Globally unique name *)
module Name : sig
  type t

  val toString : t -> string
end

(** An empty scope *)
val empty : 'v t

(** Add bindings to the scope *)
val add : (name * 'v) list -> 'v t -> 'v t

(** Return a list of all bindings *)
val bindings : 'v t -> (Name.t * name * 'v) list

(** Resolve a name into a locally unique name *)
val resolve : name -> 'v t -> Name.t option

(** Get a value by a globally unique name *)
val get : Name.t -> 'v t -> 'v option

(** Resolve name & get a corresponding value *)
val resolveAndGet : name -> 'v t -> (Name.t * 'v) option

