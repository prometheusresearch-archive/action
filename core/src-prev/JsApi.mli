
module JsResult : sig
  type 'v t

  val ok : 'v -> 'v t
  val error : string -> 'v t
  val ofResult : ('v, string) Common.Result.t -> 'v t

end

type ui
type state
type renderableState = < state : state; ui : ui Js.Nullable.t > Js.t
type query

type mutation

val pickScreen : Screen.t
val viewScreen : Screen.t

val render : state -> renderableState JsResult.t

val pickValue : Js.Json.t -> state -> renderableState JsResult.t

val id : state -> string
val uiName : ui -> string
val breadcrumbs : state -> state array
val next : state -> state array

(** Query against the current state *)
val query : state -> string -> Value.t

(** Mutate database state *)
val mutate : mutation : mutation -> value : Value.t -> state -> state
val mutationKind : mutation -> string

val db : JSONDatabase.t
val univ : JSONDatabase.Universe.t

val showQuery : Query.Typed.t -> string

val parse :
  string
  -> <
    error : string Js.Nullable.t;
    ui : renderableState Js.Nullable.t;
    data : Value.t Js.Nullable.t;
  > Js.t
