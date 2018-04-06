(**
 * This module implements a type checking / type inferrence for query syntax by
 * turning untype queries into typed ones.
 *)

type error = [ `QueryTypeError of string ]
type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

val typeQuery :
  ?ctx : Query.Typed.context
  -> univ:Universe.t
  -> Query.Untyped.t
  -> (Query.Typed.t, 'err) comp

val growQuery :
  ?bindings : Query.Typed.scope
  -> univ : Universe.t
  -> base : Query.Typed.t
  -> Query.Untyped.t
  -> (Query.Typed.t, 'err) comp

val checkArgs :
  argTyps : Query.Type.args
  -> Query.Untyped.args
  -> (Query.Untyped.args, 'err) comp

(** Same as checkArgs but doesn't set default values for missing args *)
val checkArgsPartial :
  argTyps : Query.Type.args
  -> Query.Untyped.args
  -> (Query.Untyped.args, 'err) comp

val nav :
  univ:Universe.t
  -> string
  -> Query.Typed.t
  -> (Query.Typed.t, 'err) comp
