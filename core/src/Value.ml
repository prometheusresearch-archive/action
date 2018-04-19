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

let ofCtyp ctyp =

  let registry = MutStringMap.make () in

  let rec addEntityRepr entityName entityFields =

    let add () =

      let rec repr = lazy (
        MutStringMap.set registry entityName repr;

        let fields =
          let f fields {Type. fieldName; fieldCtyp} =
            let fieldRepr = ctypRepr fieldCtyp in
            Js.Dict.set fields fieldName fieldRepr;
            fields
          in
          Belt.List.reduce entityFields (Js.Dict.empty ()) f
        in

        obj Js.Dict.(
          let dict = empty () in
          set dict "name" (string entityName);
          set dict "fields" (obj fields);
          dict
        )
      ) in
      let _ = Lazy.force repr in ()
    in

    match MutStringMap.get registry entityName with
    | None -> add ()
    | Some _ -> ()

  and cardRepr card =
    match card with
    | Query.Card.One -> string "one"
    | Query.Card.Opt -> string "opt"
    | Query.Card.Many -> string "many"

  and typRepr typ =
    let simpleType name =
      obj Js.Dict.(
        let dict = empty () in
        set dict "type" (string name);
        dict
      )
    in
    match typ with
    | Type.Void -> simpleType "void"
    | Type.Screen _ -> simpleType "screen"
    | Type.Entity { Type. entityName; entityFields } ->
      addEntityRepr entityName (entityFields typ);
      obj Js.Dict.(
        let dict = empty () in
        set dict "type" (string "entity");
        set dict "name" (string entityName);
        dict
      )

    | Type.Record fields ->
      let fields =
        let f fields {Type. fieldName;fieldCtyp} =
          let repr = ctypRepr fieldCtyp in
          Js.Dict.set fields fieldName repr;
          fields
        in
        Belt.List.reduce fields (Js.Dict.empty ()) f
      in
      let repr = obj Js.Dict.(
        let dict = empty () in
        set dict "type" (string "record");
        set dict "fields" (obj fields);
        dict
      ) in
      repr
    | Type.Value Type.String -> simpleType "string"
    | Type.Value Type.Number -> simpleType "number"
    | Type.Value Type.Bool -> simpleType "bool"
    | Type.Value Type.Null -> simpleType "null"
    | Type.Value Type.Abstract -> simpleType "abstract"

  and ctypRepr (card, typ) =
    let typRepr = typRepr typ in
    let cardRepr = cardRepr card in
    let ctypRepr = obj Js.Dict.(
      empty ()
      |> (fun o -> set o "card" cardRepr; o)
      |> (fun o -> set o "type" typRepr; o)
    ) in
    ctypRepr

  in

  let repr = ctypRepr ctyp in

  let registry =
    let f dict k v =
      Js.Dict.set dict k (Lazy.force v); dict
    in
    MutStringMap.reduce registry (Js.Dict.empty ()) f
  in

  obj Js.Dict.(
    empty ()
    |> (fun o -> set o "type" repr; o)
    |> (fun o -> set o "registry" (obj registry); o)
  )

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

