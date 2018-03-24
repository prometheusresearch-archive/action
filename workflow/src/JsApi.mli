
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

val start : renderableState JsResult.t
val render : state -> renderableState JsResult.t

val pickValue : float -> state -> renderableState JsResult.t

val id : state -> string
val uiName : ui -> string
val uiArgs : ui -> Js.Json.t Js.Dict.t
val breadcrumbs : state -> state array
val next : state -> state array

val getData : state -> Core.Value.t
val getTitle : state -> Core.Value.t

val db : Core.JSONDatabase.t
val univ : Core.Universe.t

val showQuery : Core.TypedQuery.t -> string

val parse :
  string
  -> <
    error : string Js.Nullable.t;
    ui : renderableState Js.Nullable.t;
    data : Core.Value.t Js.Nullable.t;
  > Js.t
