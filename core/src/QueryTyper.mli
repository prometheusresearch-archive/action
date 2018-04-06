(**
 * This module implements a type checking / type inferrence for query syntax by
 * turning untype queries into typed ones.
 *)

val typeQuery :
  ?ctx : Query.Typed.context
  -> univ:Universe.t
  -> Query.Untyped.t
  -> (Query.Typed.t, string) Common.Result.t

val growQuery :
  ?bindings : Query.Typed.scope
  -> univ : Universe.t
  -> base : Query.Typed.t
  -> Query.Untyped.t
  -> (Query.Typed.t, string) Common.Result.t

val checkArgs :
  argTyps : Query.Type.args
  -> Query.Untyped.args
  -> (Query.Untyped.args, string) Common.Result.t

(** Same as checkArgs but doesn't set default values for missing args *)
val checkArgsPartial :
  argTyps : Query.Type.args
  -> Query.Untyped.args
  -> (Query.Untyped.args, string) Common.Result.t

val nav :
  univ:Universe.t
  -> string
  -> Query.Typed.t
  -> (Query.Typed.t, string) Common.Result.t
