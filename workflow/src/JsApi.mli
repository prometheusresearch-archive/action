
module JsResult : sig
  type 'v t

  val ok : 'v -> 'v t
  val error : string -> 'v t
  val ofResult : ('v, string) Core.Result.t -> 'v t

end

type ui
type state
type renderableState = < state : state; ui : ui Js.Nullable.t > Js.t
type query

val render : state -> renderableState JsResult.t

val pickValue : Js.Json.t -> state -> renderableState JsResult.t

val id : state -> string
val uiName : ui -> string
val breadcrumbs : state -> state array
val next : state -> state array

(** Query against the current state *)
val query : state -> string -> Core.Value.t

val db : JSONDatabase.t
val univ : Core.Universe.t

val showQuery : Core.TypedQuery.t -> string

val parse :
  string
  -> <
    error : string Js.Nullable.t;
    ui : renderableState Js.Nullable.t;
    data : Core.Value.t Js.Nullable.t;
  > Js.t
