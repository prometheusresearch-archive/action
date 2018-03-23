(**
 * A query & workflow language based on query combinators.
 *)

(**
 * An extension to stdlib - result type which can represent a value or a
 * failure.
 *)
module Result = struct
  include Js.Result

  let ignore = function
    | Ok _ -> Ok ()
    | Error err -> Error err

  let ofOption ~err = function
    | None -> Error err
    | Some v -> Ok v

  module Syntax = struct
    let return v = Ok v
    let error err = Error err

    module Let_syntax = struct
      let bind v f = match v with
      | Ok v -> f v
      | Error err -> Error err
    end
  end

  module List = struct
    let rec map ~f =
      let open Syntax in
      function
      | [] -> return []
      | x::xs ->
        let%bind x = f x in
        let%bind xs = map ~f xs in
        return (x::xs)

    let rec foldLeft ~f ~init:v =
      let open Syntax in
      function
      | [] -> return v
      | x::xs ->
        let%bind v = f v x in
        foldLeft ~f ~init:v xs
  end

  module Array = struct
    let map ~f v =
      let open Syntax in
      let v = Array.to_list v in
      let%bind v = List.map ~f v in
      return (Array.of_list v)
  end
end

(**
 * An extension to stdlib - option type which can represent either a value or an
 * absence of it.
 *)
module Option = struct
  include Js.Option
end

(**
 * Query cardinality.
 *)
module Card = struct
  type t =
    | One
    | Opt
    | Many

  let merge a b =
    match (a, b) with
    | Many, _ -> Many

    | Opt, Many -> Many
    | Opt, _ -> Opt

    | One, Many -> Many
    | One, Opt -> Opt
    | One, One -> One

  let show = function
    | One -> "one"
    | Opt -> "opt"
    | Many -> "many"
end

(**
 * Argument is a value along with some label.
 *
 * TODO: We should only support queries as arguments.
 *)
module Arg : sig

  type t = {
    name : string;
    value : value;
  }

  and value =
    | String of string
    | Number of float
    | Bool of bool

  val string : string -> string -> t
  val number : string -> float -> t
  val bool : string -> bool -> t

  val findValueFromArgList : name : string -> t list -> value option
  val update : update : t list option -> t list option -> t list option

  val show : t -> string

end = struct

  type t = {
    name : string;
    value : value;
  }

  and value =
    | String of string
    | Number of float
    | Bool of bool

  let make name value = { name; value }

  let string name value = make name (String value)
  let number name value = make name (Number value)
  let bool name value = make name (Bool value)

  let rec findValueFromArgList ~name args =
    match args with
    | [] -> None
    | {name = argName; value}::args ->
      if name = argName
      then Some value
      else findValueFromArgList ~name args

  let update ~update args = match update, args with
    | Some update, Some args ->
      let update =
        let f update {name; value} =
          Belt.Map.String.set update name value
        in
        Belt.List.reduce update (Belt.Map.String.empty) f
      in
      let args =
        let f {name; value} =
          match Belt.Map.String.get update name with
          | None -> {name; value}
          | Some value -> {name; value}
        in
        Belt.List.map args f
      in
      Some args
    | Some update, None -> Some update
    | None, Some args -> Some args
    | None, None -> None

  let show { name; value } =
    let value = match value with
    | String value -> {j|"$value"|j}
    | Number value -> string_of_float value
    | Bool value -> string_of_bool value
    in {j|$name: $value|j}

end

(**
 * Represent available primitive value types.
 *)
module ValueType = struct
  type t =
    | StringTyp
    | NumberTyp
    | BoolTyp

  let show = function
    | StringTyp -> "string"
    | NumberTyp -> "number"
    | BoolTyp -> "bool"
end

(**
 * This defines a query syntax parametrized by the payload.
 *
 * Payload can be used to store some semantic info along with a query, for
 * example location of the query sources parsed from source files or type
 * information.
 *)
module Query (P : sig type t end) = struct

  type payload = P.t

  type t = payload * syntax

  and syntax =
    | Void
    | Here
    | Select of (t * select)
    | Navigate of t * nav
    | One of t
    | First of t
    | Chain of (t * t)
    | Screen of (t * screen)

  and nav = {
    navName : string;
    navArgs : Arg.t list option;
  }

  and screen = {
    screenName : string;
    screenArgs : Arg.t list option;
  }

  and select = field list

  and field = {
    alias : string option;
    query: t
  }

  let rec show ((_, syn) : t) =
    let showArgs = function
    | Some args ->
      let args =
        args
        |> List.map Arg.show
        |> String.concat ", "
      in {j|($args)|j}
    | None -> ""
    in
    match syn with
    | Void -> "void"
    | Here -> "here"
    | Select (parent, fields) ->
      let parent = show parent in
      let fields =
        fields
        |> List.map (function
          | {alias = None; query} -> show query
          | {alias = Some alias; query} -> let query = show query in {j|$alias: $query|j})
        |> String.concat ", "
      in
      {j|$parent { $fields }|j}
    | Navigate (parent,{ navName; navArgs; }) ->
      let navArgs = showArgs navArgs in
      let parent = show parent in
      let this = navName in
      {j|$parent.$this$navArgs|j}
    | One _ -> "one"
    | First _ -> "first"
    | Chain (parent, this) ->
      let parent = show parent in
      let this = show this in
      {j|$parent.bind($this)|j}
    | Screen (parent, { screenName; screenArgs; }) ->
      let parent = show parent in
      let screenArgs = showArgs screenArgs in
      {j|$parent.$screenName$screenArgs|j}
end

(**
 * Untype query.
 *)
module UntypedQuery = struct

  include Query(struct
    type t = unit
  end)

  (**
   * A set of combinators to construct queries programmatically.
   *)
  module Syntax = struct

    let void =
      (), Void

    let here =
      (), Here

    let nav ?args name parent =
      (), Navigate (parent, { navName = name; navArgs = args; })

    let chain q parent =
      (), Chain (parent, q)

    let select fields parent =
      (), Select (parent, fields)

    let field ?alias query =
      { query; alias; }

    let screen ?args name query =
      (), Screen (query, { screenName = name; screenArgs = args; })

    let one query =
      (), One query

    let first query =
      (), First query

  end

end

(**
 * Type system.
 *)
module Type = struct

  type t =
    | Void
    | Screen of screen
    | Entity of entity
    | Record of field list
    | Value of value

  and ct = Card.t * t

  and screen = {
    screenName : string;
    screenFields : field list;
    screenNavigate : string;
  }

  and value = {
    valueTyp: ValueType.t;
  }

  and entity = {
    entityName : string;
    entityFields : field list option;
  }

  and field = {
    fieldName : string;
    fieldArgs : argTyp list option;
    fieldCtyp : Card.t * t;
  }

  and argTyp =
    | Req of argTypInfo
    | Opt of argTypInfo

  and argTypInfo = {
    argName : string;
    argTyp : ValueType.t;
  }

  let rec show = function
    | Void -> "void"
    | Value {valueTyp; _} -> ValueType.show valueTyp;
    | Screen { screenName; screenFields = _; } -> {j|Screen("$screenName")|j}
    | Entity {entityName; _} -> {j|Entity($entityName)|j}
    | Record fields ->
      let fields =
        fields
        |> List.map (fun { fieldName; fieldArgs = _; fieldCtyp } ->
          let fieldCtyp = showCt fieldCtyp in
          {j|$fieldName: $fieldCtyp|j})
        |> String.concat ", "
      in
      {j|{ $fields }|j}

  and showCt (card, typ) =
    let card = Card.show card in
    let typ = show typ in
    {j|$card $typ|j}

  (**
   * Combinators to define a type system.
   *)
  module Syntax = struct

    let entity name fields = Entity {entityName = name; entityFields = Some fields}

    let has ?(card=Card.One) ?args name typ =
      {
        fieldName = name;
        fieldArgs = args;
        fieldCtyp = card, typ;
      }

    let hasOne = has ~card:Card.One
    let hasOpt = has ~card:Card.Opt
    let hasMany = has ~card:Card.Many

    module Value = struct
      let string = Value { valueTyp = ValueType.StringTyp; }
      let number = Value { valueTyp = ValueType.NumberTyp; }
      let bool = Value { valueTyp = ValueType.BoolTyp; }
    end

    include Value

    let requiredArg name typ =
      Req {argName = name; argTyp = typ}

    let optionalArg name typ =
      Opt {argName = name; argTyp = typ}
  end

end


(**
 * Query with type and cardinality information attached.
 *)
module TypedQuery = struct
  include Query(struct
    type t = Type.ct
  end)

  let void = (Card.One, Type.Void), Void
end

(**
 * Query result which extends JSON type with a special UI type.
 *
 * It is implemented as a zero (almost) cost on top of native JS data
 * structures.
 *)
module Value = struct

  type t
  type queryResult = t

  (**
  * This is an opaque structure which defines UI.
  *)
  module UI : sig

    type t

    val make :
      name : string
      -> args : Arg.t list option
      -> typ : Type.t
      -> queryResult
      -> TypedQuery.t
      -> t

    val test : 'a -> bool

    val name : t -> string
    val args : t -> Arg.t list option
    val setArgs : args : Arg.t list option -> t -> t
    val query : t -> TypedQuery.t
    val value : t -> queryResult
    val typ : t -> Type.t

  end = struct
    type t = <
      name : string;
      args : Arg.t list option;
      query : TypedQuery.t;
      value : queryResult;
      typ : Type.t;
    > Js.t

    external make : name : string -> args : Arg.t list option -> typ : Type.t -> queryResult -> TypedQuery.t -> t =
      "UIRepr" [@@bs.new] [@@bs.module "./UIRepr"]

    let test_ : 'a -> bool = [%bs.raw {|
      function test(v) { return v instanceof UIRepr.UIRepr; }
    |}]

    let test x = test_ (Obj.magic x)

    let name ui = ui##name
    let args ui = ui##args
    let query ui = ui##query
    let value ui = ui##value
    let typ ui = ui##typ

    let setArgs ~args ui =
      let args = Arg.update ~update:args ui##args in
      make ~name:ui##name ~args ~typ:ui##typ ui##value ui##query
  end

  let null : t = Obj.magic (Js.null)
  external string : string -> t = "%identity"
  external number : float -> t = "%identity"
  external bool : bool -> t = "%identity"
  external ui : UI.t -> t = "%identity"
  external array : t array -> t = "%identity"
  external obj : t Js.Dict.t -> t = "%identity"
  external ofJson : Js.Json.t -> t = "%identity"

  let ofOption = function
    | Some v -> v
    | None -> null

  type tagged =
    | Object of t Js.Dict.t
    | Array of t array
    | String of string
    | Number of float
    | Bool of bool
    | UI of UI.t
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
    else Object (Obj.magic v)

end

module Screen = struct

  type t = {
    args : Arg.t list option;
    fields : Type.t -> (Type.field * fieldResolver) list;
    inputCard : Card.t;
    navigate : string;
  }

  and fieldResolver =
    screenArgs : Arg.t list
    -> args : Arg.t list
    -> Value.t
    -> (Value.t, string) Result.t

  let typ ~name ~typ screen =
    let fields = Belt.List.map (screen.fields typ) (fun (field, _) -> field)
    in Type.Screen { screenName = name; screenFields = fields; screenNavigate = screen.navigate }

  let lookupField ~name ~typ screen =
    Belt.List.getBy (screen.fields typ) (fun (field, _) -> field.Type.fieldName = name)

  module Syntax = struct

    include Type.Syntax.Value

    let screen ?args ~inputCard ~navigate fields = {
      fields;
      args;
      inputCard;
      navigate;
    }

    let has ?(card=Card.One) ?args ~resolve name typ =
      let field = {
        Type.
        fieldName = name;
        fieldArgs = args;
        fieldCtyp = card, typ;
      } in field, resolve
  end

end

(**
 * Universe represent system configuration and is used to resolve queries
 * available on void type and screen types available.
 *)
module Universe : sig

  type t

  (**
   * An empty universe.
   *)
  val empty : t

  val hasOne : ?args : Type.argTyp list -> string -> Type.t -> t -> t
  val hasOpt : ?args : Type.argTyp list -> string -> Type.t -> t -> t
  val hasMany : ?args : Type.argTyp list -> string -> Type.t -> t -> t

  val hasScreen : string -> Screen.t -> t -> t

  val fields : t -> Type.field list

  val lookupScreen : string -> t -> Screen.t option

end = struct

  module Map = Belt.Map.String

  type t = {
    fields : Type.field list;
    screens : Screen.t Map.t;
  }

  let empty = { fields = []; screens = Map.empty }

  let hasOne ?args name typ univ =
    let field = Type.Syntax.hasOne ?args name typ in
    { univ with fields = field::univ.fields }

  let hasOpt ?args name typ univ =
    let field = Type.Syntax.hasOpt ?args name typ in
    { univ with fields = field::univ.fields }

  let hasMany ?args name typ univ =
    let field = Type.Syntax.hasMany ?args name typ in
    { univ with fields = field::univ.fields }

  let hasScreen name screen univ =
    { univ with screens = Map.set univ.screens name screen; }

  let fields univ = univ.fields

  let lookupScreen name univ =
    Map.get univ.screens name
end

(**
 * This module implements a type checking / type inferrence for query structure
 * by turning untype queries into typed ones.
 *)
module QueryTyper : sig

  val typeQuery :
    ?ctyp : TypedQuery.payload
    -> univ:Universe.t
    -> UntypedQuery.t
    -> (TypedQuery.t, string) Result.t

  val nav :
    univ:Universe.t
    -> ?args : Arg.t list
    -> string
    -> TypedQuery.t
    -> (TypedQuery.t, string) Result.t

end = struct

  let extractField ~univ fieldName (typ : Type.t) =
    let open Result.Syntax in
    let findInFieldList fields =
      match Belt.List.getBy fields (fun field -> field.Type.fieldName = fieldName) with
      | None ->
        let typ = Type.show typ in
        error {j|no such field "$fieldName" on $typ|j}
      | Some field -> Ok field
    in
    match typ with
    | Type.Void -> let fields = Universe.fields univ in findInFieldList fields
    | Type.Screen { screenFields; _ } -> findInFieldList screenFields
    | Type.Entity {entityName = _; entityFields = None} -> error "cannot extract field"
    | Type.Entity {entityName = _; entityFields = Some fields} -> findInFieldList fields
    | Type.Record fields -> findInFieldList fields
    | Type.Value _ -> error "cannot extract field"

  let rootCtyp = Card.One, Type.Void

  let nav ~univ ?args name parent =
    let open Result.Syntax in
    let navigation = { TypedQuery. navName = name; navArgs = args; } in
    let (parentCard, parentTyp), _parentSyn = parent in
    let%bind field = extractField ~univ name parentTyp in
    let fieldCard, fieldTyp = field.fieldCtyp in
    let fieldCard = Card.merge parentCard fieldCard in
    return ((fieldCard, fieldTyp), TypedQuery.Navigate (parent, navigation))

  let typeQuery ?(ctyp=rootCtyp) ~univ query =
    let rec aux ~ctyp ((), query) =
      let open Result.Syntax in
      match query with
      | UntypedQuery.Void ->
        return ((Card.One, Type.Void), TypedQuery.Void)
      | UntypedQuery.Here ->
        return (ctyp, TypedQuery.Here)
      | UntypedQuery.Chain (parent, q) ->
        let%bind ((prevCard, _) as ctyp, _) as parent = aux ~ctyp parent in
        let%bind ((card, typ), _) as q = aux ~ctyp q in
        let ctyp = Card.merge prevCard card, typ in
        return (ctyp, TypedQuery.Chain (parent, q))
      | UntypedQuery.One parent ->
        let%bind parent = aux ~ctyp parent in
        return (ctyp, TypedQuery.One parent)
      | UntypedQuery.First parent ->
        let%bind ((_, parentType), _) as parent = aux ~ctyp parent in
        return ((Card.Opt, parentType), TypedQuery.One parent)
      | UntypedQuery.Screen (parent, { screenName; screenArgs; }) ->
        let%bind ((parentCard, parentTyp), _) as parent = aux ~ctyp parent in
        let%bind screen = Result.ofOption
          ~err:{j|no such screen "$screenName"|j}
          (Universe.lookupScreen screenName univ)
        in begin match (screen.inputCard, parentCard) with
        | Card.One, Card.Many
        | Card.Opt, Card.Many
        | Card.Many, Card.One
        | Card.Many, Card.Opt ->
          error "screen cannot be constructed due to cardinality mismatch"
        | Card.One, Card.Opt
        | Card.One, Card.One
        | Card.Opt, Card.Opt
        | Card.Opt, Card.One
        | Card.Many, Card.Many ->
          let typ = Screen.typ ~name:screenName ~typ:parentTyp screen in
          return ((Card.One, typ), TypedQuery.Screen (parent, { screenName; screenArgs; }))
        end
      | UntypedQuery.Navigate (parent, navigation) ->
        let { UntypedQuery. navName; navArgs } = navigation in
        let%bind parent = aux ~ctyp parent in
        nav ~univ ?args:navArgs navName parent
      | UntypedQuery.Select (parent, selection) ->
        let%bind parent = aux ~ctyp parent in
        let parentCtyp, _parentSyn = parent in
        let parentCard, _parentTyp = parentCtyp in
        let checkField fields { UntypedQuery. alias; query } =
          match fields with
          | Result.Ok (fields, selection, index) ->
            let%bind query = aux ~ctyp:parentCtyp query in
            let (fieldCard, fieldTyp), _ = query in
            let fieldName = Option.getWithDefault (string_of_int index) alias in
            let fieldCard = Card.merge parentCard fieldCard in
            let fieldCtyp = fieldCard, fieldTyp in
            let field = { Type. fieldCtyp; fieldName; fieldArgs = None } in
            let selectionField = { TypedQuery. alias; query; } in
            Result.Ok (field::fields, selectionField::selection, index + 1)
          | Result.Error err ->
            error err
        in
        let%bind (fields, selection, _) =
          let init = Result.Ok ([], [], 0) in
          Belt.List.reduce selection init checkField
        in
        let typ = Type.Record fields in
        return ((parentCard, typ), TypedQuery.Select (parent, selection))
    in aux ~ctyp query

end

(**
 * Abstract interface to the database.
 *)
module type DATABASE = sig

  type t

  val univ : t -> Universe.t

  val execute : t -> TypedQuery.t -> (Value.t, string) Result.t

end

(**
 * A database for an in-memory JSON objects.
 *)
module JSONDatabase : sig
  include DATABASE

  val ofString : univ : Universe.t -> string -> t
  val ofJson : univ : Universe.t -> Js.Json.t -> t
end = struct

  type t = {
    root : Value.t;
    univ : Universe.t;
  }

  let ofString ~univ data =
    let root = Js.Json.parseExn data in
    { root = Value.ofJson root; univ }

  let ofJson ~univ root = { root = Value.ofJson root; univ }

  let univ db = db.univ

  let execute db query =
    Js.log2 "EXECUTE" (TypedQuery.show query);
    let open Result.Syntax in
    let rec aux ~(value : Value.t) ((_card, typ), syn) =
      match syn with
      | TypedQuery.Void -> return db.root
      | TypedQuery.Here ->
        return value
      | TypedQuery.Chain (query, next) ->
        let%bind value = aux ~value query in
        let%bind value = aux ~value next in
        return value
      | TypedQuery.One query ->
        let%bind value = aux ~value query in
        begin match Value.classify value with
        | Value.Array items ->
          if Array.length items = 1
          then return (Array.get items 0)
          else error "expected a single value but got multiple"
        | Value.Null -> error "expected a single value but got null"
        | _ -> return value
        end
      | TypedQuery.First query ->
        let%bind value = aux ~value query in
        begin match Value.classify value with
        | Value.Array items ->
          if Array.length items = 1
          then return (Array.get items 0)
          else return Value.null
        | _ -> return value
        end
      | TypedQuery.Screen (q, { screenName; screenArgs; }) ->
        begin match Value.classify value with
        | Value.Null -> return Value.null
        | _ ->
          return (Value.ui (Value.UI.make ~name:screenName ~args:screenArgs ~typ value q))
        end
      | TypedQuery.Navigate (query, { navName; navArgs }) ->
        let args = Option.getWithDefault [] navArgs in
        let%bind value = aux ~value query in
        let navigate name dataset =
          match Value.classify dataset with
          | Value.Object obj ->
            begin match Js.Dict.get obj name with
            | Some dataset -> return dataset
            | None ->
              let msg = {j|no such key "$name"|j} in
              Js.log3 "ERROR:" msg [%bs.obj { data = dataset; key = name; }];
              error msg
            end
          | _ -> error "expected an object"
        in begin
        match Value.classify value with
        | Value.Object _ -> navigate navName value
        | Value.Array items  ->
          let%bind items = Result.Array.map ~f:(navigate navName) items in
          return (Value.array items)
        | Value.Null ->
          return Value.null
        | Value.UI ui ->
          let query = Value.UI.query ui in
          let queryValue = Value.UI.value ui in
          let%bind value = aux ~value:queryValue query in
          let screenName = Value.UI.name ui in
          let screenArgs = Option.getWithDefault [] (Value.UI.args ui) in
          let%bind screen = Result.ofOption
            ~err:{j|no such screen "$screenName"|j}
            (Universe.lookupScreen screenName db.univ)
          in
          let%bind _, resolve = Result.ofOption
            ~err:{j|no such field "$navName"|j}
            (Screen.lookupField ~name:navName ~typ:(Value.UI.typ ui) screen)
          in resolve ~screenArgs ~args value
        | _ -> error {|Cannot navigate away from this value|}
        end
      | TypedQuery.Select (query, selection) ->
        let%bind value = aux ~value query in
        let%bind _, dataset =
          let build state { TypedQuery. alias; query; } =
            match state with
            | Result.Ok (idx, dataset) ->
              let%bind selectionValue = aux ~value query in
              let selectionAlias = Option.getWithDefault (string_of_int idx) alias in
              Js.Dict.set dataset selectionAlias selectionValue;
              return (idx + 1, dataset)
            | Result.Error err -> error err
          in
          Belt.List.reduce selection (Result.Ok (0, Js.Dict.empty ())) build
        in return (Value.obj dataset)
    in
    let%bind res = aux ~value:db.root query in
    Js.log2 "EXECUTE RESULT" res;
    return res

end

(**
 * Monadic structure on top queries which represent transition between screens.
 *)
module Workflow (Q : sig type t end) = struct

  type q = Q.t
  type t =
    (** Render concrete query to a screen *)
    | Render of q
    (** Define how to transition from one screen to another screen *)
    | Next of (t * t list)

end

module UntypedWorkflow = struct
  include Workflow(struct
    type t = UntypedQuery.t
  end)

  module Syntax = struct
    let render q = Render q
    let andThen path w = Next (w, path)
  end
end

module TypedWorkflow = struct
  include Workflow(struct
    type t = (TypedQuery.t * string)
  end)
end

module WorkflowTyper = struct

  let rootCtyp = Card.One, Type.Void

  let typeWorkflow ~univ w =
    let open Result.Syntax in
    let rec aux ~ctyp w =
      match w with
      | UntypedWorkflow.Render q ->
        let%bind ((_, typ) as ctyp, _) as tq = QueryTyper.typeQuery ~univ ~ctyp q in
        begin match typ with
        | Type.Void | Type.Entity _ | Type.Record _ | Type.Value _ ->
          let typ = Type.show typ in
          let msg = {j|workflow can only be defined with screen but got $typ|j} in
          error msg
        | Type.Screen { screenNavigate; _ } ->
          let q = UntypedQuery.Syntax.(here |> nav screenNavigate) in
          let%bind screenOutputCtyp, _ = QueryTyper.typeQuery ~univ ~ctyp q in
          return (TypedWorkflow.Render (tq, screenNavigate), screenOutputCtyp)
        end
      | UntypedWorkflow.Next (first, next) ->
        let%bind first, ctyp = aux ~ctyp first in
        let%bind next, _ =
          let f (next, ctyp) w =
            let%bind w, _ = aux ~ctyp w in
            return (w::next, ctyp)
          in
          Result.List.foldLeft ~f ~init:([], ctyp) next
        in
        return (TypedWorkflow.Next (first, List.rev next), ctyp)
    in
    let%bind tw, _ = aux w ~ctyp:rootCtyp in
    return tw

end

module WorkflowInterpreter (Db : DATABASE) : sig

  (**
   * This type represents workflow execution state.
   *)
  type t

  (**
   * Produce an initial state for the given database and workflow description.
   *)
  val boot : db : Db.t -> TypedWorkflow.t -> ((t * Value.UI.t), string) Result.t

  (**
   * Render workflow state and return a new state and a UI screen to render.
   *)
  val render : t -> ((t * Value.UI.t), string) Result.t

  val dataQuery : t -> (TypedQuery.t, string) Result.t

  val titleQuery : t -> (TypedQuery.t, string) Result.t

  val setArgs : args : Arg.t list option -> t -> t

  val step : t -> (t, string) Result.t

  val show : t -> string

  val breadcrumbs : t -> t list

  val next : t -> (t list, string) Result.t

end = struct

  type t = frame * Value.UI.t option

  and frame = {
    db : Db.t;
    query : TypedQuery.t;
    workflow : TypedWorkflow.t;
    position: position;
    prev: t option;
    args : Arg.t list option;
  }

  and position =
    | Root
    | First of t
    | Next of t

  let uiQuery (frame, _) =
    let open Result.Syntax in
    match frame.workflow with
    | TypedWorkflow.Render (q, _) ->
      let ctyp, _ = q in
      let q = ctyp, TypedQuery.Chain (frame.query, q) in
      return q
    | _ -> error "no query"

  let rec breadcrumbs (frame, _ as state) =
    match frame.prev with
    | Some prev -> state::(breadcrumbs prev)
    | None -> [state]

  let rec show (frame, _ as state) =
    let query = match uiQuery state with
    | Result.Ok query -> TypedQuery.show query
    | Result.Error _ -> "EMPTY"
    in
    match frame.prev with
    | None -> {j|$query <- ROOT|j}
    | Some prev -> let prev = show prev in {j|$query <- $prev|j}

  let make ?prev ?ui ?args ~position ?(query=TypedQuery.void) ~db workflow =
    let frame = {
      query;
      db;
      workflow;
      position;
      prev;
      args;
    } in frame, ui

  let findRender (frame, _ as state) =
    let open Result.Syntax in

    let rec find (frame, _ as state) =
      let {workflow; db; query; _} = frame in
      match workflow with
      | TypedWorkflow.Next (first, _next) ->
        let state = make ~position:(First state) ~query ~db first in
        find state
      | TypedWorkflow.Render _ ->
        return state
    in

    match frame.workflow with
    | TypedWorkflow.Render _ -> return state
    | _ -> find state


  let render state =
    let open Result.Syntax in

    let render (frame, _ as state) =
      let%bind q = uiQuery state in
      Js.log2 "WorkflowInterpreter.render" (TypedQuery.show q);
      let%bind res = Db.execute frame.db q in
      match Value.classify res, frame.args with
      | Value.UI ui, Some args ->
        let ui = Value.UI.setArgs ~args:(Some args) ui in
        return ((frame, Some ui), ui)
      | Value.UI ui, None ->
        return ((frame, Some ui), ui)
      | _ -> error "expected UI, got data"
    in

    let%bind state = findRender state in
    render state

  let boot ~db workflow =
    let open Result.Syntax in
    let state = make ~position:Root ~db workflow in
    Js.log2 "WorkflowInterpreter.boot: init:" (show state);
    let%bind state, ui = render state in
    Js.log2 "WorkflowInterpreter.boot: first render:" (show state);
    return (state, ui)

  let next (startFrame, _ as currentState) =
    let open Result.Syntax in

    let rec aux query (frame, _ as state) =
      let {workflow; position; db;} = frame in
      match workflow, position with
      | TypedWorkflow.Render _, First parent -> aux query parent
      | TypedWorkflow.Render _, Next _  -> return []
      | TypedWorkflow.Render _, Root -> return []
      | TypedWorkflow.Next (_first, []), First parent -> aux query parent
      | TypedWorkflow.Next (_first, _), Next _ -> return []
      | TypedWorkflow.Next (_first, next), _ ->
        let f w =
          let state = make ~prev:currentState ~query ~position:(Next state) ~db w in
          findRender state
        in
        Result.List.map ~f next
    in

    (* First we need to produce the output query for the current state *)
    let%bind q = uiQuery currentState in
    let%bind q =
      match startFrame.workflow with
      | Render (_, navigate) ->
        let univ = Db.univ startFrame.db in
        QueryTyper.nav ~univ navigate q
      | _ -> return q
    in

    aux q currentState

  let step state =
    let open Result.Syntax in
    match%bind next state with
    | [] -> return state
    | state::_ -> return state

  let dataQuery (frame, _ as state) =
    let open Result.Syntax in
    let univ = Db.univ frame.db in
    let%bind q = uiQuery state in
    let%bind q = QueryTyper.nav ~univ "data" q in
    return q

  let titleQuery (frame, _ as state) =
    let open Result.Syntax in
    let univ = Db.univ frame.db in
    let%bind q = uiQuery state in
    let%bind q = QueryTyper.nav ~univ "title" q in
    return q

  let setArgs ~args (frame, ui) =
    let workflow = match frame.workflow with
    | TypedWorkflow.Render ((ct, TypedQuery.Screen (p, c)), n) ->
      let c = { c with TypedQuery. screenArgs = Arg.update ~update:args c.screenArgs } in
      TypedWorkflow.Render ((ct, TypedQuery.Screen (p, c)), n)
    | _ -> frame.workflow
    in
    let frame = { frame with args; workflow; } in
    (frame, ui)

end

module JsResult : sig
  type 'v t

  val ok : 'v -> 'v t
  val error : string -> 'v t
  val ofResult : ('v, string) Result.t -> 'v t

end = struct

  type 'v t

  let ok value = Obj.magic [%bs.obj {_type = "Ok"; value;}]
  let error error = Obj.magic [%bs.obj {_type = "Error"; error;}]

  let ofResult = function
    | Result.Ok v -> ok v
    | Result.Error err -> error err

end

(**
 * JS API
 *)
module JsApi : sig

  type ui
  type state
  type query
  type uquery

  val start : < state : state; ui : ui > Js.t JsResult.t
  val renderState : state -> < state : state; ui : ui > Js.t JsResult.t

  val pickValue : float -> state -> < state : state; ui : ui > Js.t JsResult.t

  val uiName : ui -> string
  val breadcrumbs : state -> state array
  val next : state -> state array

  val getData : state -> Value.t
  val getTitle : state -> Value.t

  val db : JSONDatabase.t
  val univ : Universe.t

  val showQuery : TypedQuery.t -> string

end = struct
  module WorkflowInterpreter = WorkflowInterpreter(JSONDatabase)

  type ui = Value.UI.t
  type state = WorkflowInterpreter.t
  type query = TypedQuery.t
  type uquery = UntypedQuery.t

  let showQuery = TypedQuery.show

  let uiName = Value.UI.name

  let pickScreen =
    let resolveData ~screenArgs:_ ~args:_ value =
      let open Result.Syntax in
      return value
    in
    let resolveValue ~screenArgs ~args:_ value =
      let id = Arg.findValueFromArgList ~name:"id" screenArgs in
      match id, Value.classify value with
      | None, _ ->
        Result.Ok Value.null
      | Some (Arg.Number id), Value.Array value ->
        Result.Ok (
          if Array.length value = 0
          then Value.null
          else
            let f value = match Value.classify value with
            | Value.Object obj -> begin match Js.Dict.get obj "id" with
              | Some v -> (Obj.magic v) = id
              | None -> false
              end
            | _ -> false
            in
            Value.ofOption (Js.Array.find f value))
      | _ -> Result.Error "invalid invocation"
    in
    let resolveTitle ~screenArgs ~args:_ value =
      let open Result.Syntax in
      let%bind value = resolveValue ~screenArgs ~args:[] value in
      match Value.classify value with
      | Value.Object obj ->
        begin match Js.Dict.get obj "id" with
        | None -> Result.Ok (Value.string "Pick Screen")
        | Some id -> Result.Ok (Value.string {j|Pick Screen ($id)|j})
        end
      | _ -> Result.Ok (Value.string "Pick Screen")
    in
    Screen.Syntax.(screen
      ~inputCard:Card.Many
      ~navigate:"value"
      (fun entity -> [
        has ~resolve:resolveTitle "title" string;
        has ~resolve:resolveValue ~card:Card.Opt "value" entity;
        has ~resolve:resolveData ~card:Card.Many "data" entity;
      ])
    )

  let viewScreen =
    let resolveData ~screenArgs:_ ~args:_ value =
      let open Result.Syntax in
      return value
    in
    let resolveValue ~screenArgs:_ ~args:_ value =
      Result.Ok value
    in
    let resolveTitle ~screenArgs ~args:_ value =
      let open Result.Syntax in
      let%bind value = resolveValue ~screenArgs ~args:[] value in
      match Value.classify value with
      | Value.Object obj ->
        begin match Js.Dict.get obj "id" with
        | None -> Result.Ok (Value.string "View Screen")
        | Some id -> Result.Ok (Value.string {j|View Screen ($id)|j})
        end
      | _ -> Result.Ok (Value.string "View Screen")
    in
    Screen.Syntax.(screen
      ~inputCard:Card.One
      ~navigate:"value"
      (fun entity -> [
        has ~resolve:resolveTitle "title" string;
        has ~resolve:resolveValue ~card:Card.Opt "value" entity;
        has ~resolve:resolveData ~card:Card.Opt "data" entity;
      ])
    )

  let univ =

    let site = Type.Syntax.(entity "site" [
      hasOne "title" string;
    ]) in

    let individual = Type.Syntax.(entity "individual" [
      hasOne "name" string;
      hasOne "site" site;
    ]) in

    Universe.(
      empty
      |> hasMany "individual" individual
      |> hasScreen "pick" pickScreen
      |> hasScreen "view" viewScreen
    )

  let db = JSONDatabase.ofString ~univ {|
    {
      "individual": [
        {
          "id": 1,
          "name": "Andrey Popp",
          "site": {
            "title": "RexDB Site"
          }
        },
        {
          "id": 2,
          "name": "Oleksiy Golovko",
          "site": {
            "title": "Portal Site"
          }
        },
        {
          "id": 3,
          "name": "Clark Evans",
          "site": {
            "title": "Portal Site"
          }
        }
      ]
    }
  |}


  let workflow =
    let open UntypedWorkflow.Syntax in
    let open UntypedQuery.Syntax in

    let pickIndividual = render (here |> nav "individual" |> screen "pick") in
    let view = render (here |> screen "view") in
    let viewSite = render (here |> nav "site" |> screen "view") in

    pickIndividual |> andThen [
      view;
      viewSite;
    ]

  let start =
    let v =
      let open Result.Syntax in
      let%bind w = WorkflowTyper.typeWorkflow ~univ workflow in
      let%bind state, ui = WorkflowInterpreter.boot ~db w in
      return [%bs.obj { state; ui }]
    in JsResult.ofResult v

  let toJS state =
    JsResult.ofResult (
      let open Result.Syntax in
      let%bind state, ui = state in
      Result.Ok [%bs.obj { state; ui }]
    )

  let breadcrumbs state =
    state |> WorkflowInterpreter.breadcrumbs |> Array.of_list

  let next state =
    match WorkflowInterpreter.next state with
    | Ok next -> Array.of_list next
    | Error err -> Js.Exn.raiseError err

  let renderState state =
    toJS (WorkflowInterpreter.render state)

  let pickValue id state =
    toJS (
      let open Result.Syntax in
      let args = Some [Arg.number "id" id] in
      let state = WorkflowInterpreter.setArgs ~args state in
      let%bind state = WorkflowInterpreter.step state in
      WorkflowInterpreter.render state
    )

  let executeQuery q =
    let res =
      let open Result.Syntax in
      let%bind q = q in
      let%bind data = JSONDatabase.execute db q in
      return data
    in match res with
    | Result.Ok data -> data
    | Result.Error err -> Js.Exn.raiseError err

  let getData state =
    executeQuery (WorkflowInterpreter.dataQuery state)

  let getTitle state =
    executeQuery (WorkflowInterpreter.titleQuery state)

end

include JsApi

