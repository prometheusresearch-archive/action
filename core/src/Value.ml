(**
 * Query result which extends JSON type with a special UI type.
 *
 * It is implemented as a zero (almost) cost on top of native JS data
 * structures.
 *)

module StringMap = Common.StringMap
module MutStringMap = Common.MutStringMap
module Result = Common.Result
module Type = Query.Type

type t
type value = t

(**
* This is an opaque structure which defines UI.
*)
module UI : sig

  type t

  val make :
    screen : Screen.t
    -> name : string
    -> args : Query.Untyped.args
    -> typ : Type.t
    -> value : value
    -> parentQuery : Query.Typed.t
    -> t

  val test : 'a -> bool

  val name : t -> string
  val screen : t -> Screen.t
  val args : t -> Query.Untyped.args
  val setArgs : args : Query.Untyped.args -> t -> t
  val parentQuery : t -> Query.Typed.t
  val value : t -> value
  val typ : t -> Type.t

end = struct
  type t = <
    screen : Screen.t;
    name : string;
    args : Query.Untyped.args;
    parentQuery : Query.Typed.t;
    value : value;
    typ : Type.t;
  > Js.t

  external make :
    screen : Screen.t
    -> name : string
    -> args : Query.Untyped.args
    -> typ : Type.t
    -> value : value
    -> parentQuery : Query.Typed.t
    -> t
    = "UIRepr" [@@bs.new] [@@bs.module "./UIRepr"]

  let test_ : 'a -> bool = [%bs.raw {|
    function test(v) { return v instanceof UIRepr.UIRepr; }
  |}]

  let test x = test_ (Obj.magic x)

  let name ui = ui##name
  let args ui = ui##args
  let screen ui = ui##screen
  let parentQuery ui = ui##parentQuery

  let value ui = ui##value
  let typ ui = ui##typ

  let setArgs ~args ui =
    let args = Query.Untyped.updateArgs ~update:args ui##args in
    make
      ~screen:ui##screen
      ~name:ui##name
      ~args
      ~typ:ui##typ
      ~value:ui##value
      ~parentQuery:ui##parentQuery

end

let null : t = Obj.magic (Js.null)
external string : string -> t = "%identity"
external number : float -> t = "%identity"
external bool : bool -> t = "%identity"
external ui : UI.t -> t = "%identity"
external mutation : (t, 'err) Mutation.t -> t = "%identity"
external array : t array -> t = "%identity"
external obj : t Js.Dict.t -> t = "%identity"
external ofJson : Js.Json.t -> t = "%identity"

let ofOption = function
  | Some v -> v
  | None -> null

type 'err tagged =
  | Object of t Js.Dict.t
  | Array of t array
  | String of string
  | Number of float
  | Bool of bool
  | UI of UI.t
  | Mutation of (t, 'err) Mutation.t
  | Null

let classify (v : t) =
  if Js.typeof v = "string"
  then String (Obj.magic v)
  else if Js.typeof v = "number"
  then Number (Obj.magic v)
  else if Js.typeof v = "boolean"
  then Bool (Obj.magic v)
  else if Obj.magic v == Js.null
  then Null
  else if Js.Array.isArray (Obj.magic v)
  then Array (Obj.magic v)
  else if UI.test v
  then UI (Obj.magic v)
  else if Mutation.test v
  then Mutation (Obj.magic v)
  else Object (Obj.magic v)

let decodeObj v =
  match classify v with
  | Object obj -> Some obj
  | _ -> None

let decodeString v =
  match classify v with
  | String v -> Some v
  | _ -> None

let decodeMutation v =
  match classify v with
  | Mutation v -> Some v
  | _ -> None

let get ~name value =
  match classify value with
  | Object value -> Js.Dict.get value name
  | _ -> None

