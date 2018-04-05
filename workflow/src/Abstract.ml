(**
 * This module contains abstract interface declarations for the action.
 *)

(**
 * Database
 *)
module type DATABASE = sig

  type t

  (**
   * Universe associated with the database.
   *)
  val univ : t -> Core.Universe.t

  val root : t -> Core.Value.t

  (**
   * Execute typed query.
   *)
  val execute :
    ?value : Core.Value.t
    -> db : t
    -> Core.TypedQuery.t
    -> (Core.Value.t, string) Core.Result.t

end
