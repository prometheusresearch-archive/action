(**
 * Universe represent system configuration and is used to resolve queries
 * available on void type and screen types available.
 *)

type t

(**
  * An empty universe.
  *)
val empty : t

val hasOne : ?args : Query.Type.Syntax.arg list -> string -> Query.Type.t -> t -> t
val hasOpt : ?args : Query.Type.Syntax.arg list -> string -> Query.Type.t -> t -> t
val hasMany : ?args : Query.Type.Syntax.arg list -> string -> Query.Type.t -> t -> t

val hasScreen : string -> Screen.t -> t -> t

val fields : t -> Query.Type.field list

val lookupScreen : string -> t -> Screen.t option

val lookupScreenResult : string -> t -> (Screen.t, string) Common.Result.t
