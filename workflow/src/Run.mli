(**
 * Run is a computation which can either result in some value or fail.
 *
 * The difference between Run and result is that Run keeps the rich error
 * context which is presented upon failure.
 *)

(**
 * A computation which either computes 'a value or fails. 'errctx models the
 * context of the computation and is presented upon failure.
 *)
type ('v, 'errctx) t

(**
 * Construct a new computation by wrapping an already present 'a value.
 *)
val return : 'v -> ('v, 'errctx) t

(**
 * Construct a failed computation given the error message.
 *)
val error : 'errctx -> ('a, 'errctx) t

(**
 * Provide context to the computation.
 *)
val context : 'errctx -> ('a, 'errctx) t -> ('a, 'errctx) t

val toResult : ('a, 'errctx) t -> ('a, 'errctx) Js.Result.t

(**
 * A module which is designed to be openned locally for functions which perform
 * Run compiutations:
 *
 *    let open Run.Syntax in
 *    let%bind x = ... in
 *    return (x + 1)
 *
 *)
module Syntax : sig

  (** Re-export *)
  val return : 'a -> ('a, 'errctx) t
  (** Re-export *)
  val error : 'errctx -> ('a, 'errctx) t
  (** Re-export *)
  val context : 'errctx -> ('a, 'errctx) t -> ('a, 'errctx) t

  (**
   * Syntax for ppx_let.
   *)
  module Let_syntax : sig

    val bind : ('a, 'errctx) t -> ('a -> ('b, 'errctx) t) -> ('b, 'errctx) t
  end
end

(**
 * Utilities to deal with lists of Run computations.
 *)
module List : sig

  val map : f:('a -> ('b, 'errctx) t) -> 'a list -> ('b list, 'errctx) t

  val foldLeft : f:('a -> 'b -> ('a, 'errctx) t) -> init:'a -> 'b list -> ('a, 'errctx) t
end

(**
 * Utilities to deal with arrays of Run computations.
 *)
module Array : sig

  val map : f:('a -> ('b, 'errctx) t) -> 'a array -> ('b array, 'errctx) t
end
