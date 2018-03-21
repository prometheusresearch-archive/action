(**
 * Rabbit based UI query language.
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

  let compare a b =
    match (a, b) with
    | Many, Many -> 0
    | Many,Opt -> -1
    | Many,One -> -1
    | Opt, Many -> 1
    | Opt, Opt -> 0
    | Opt, One -> -1
    | One,Many -> 1
    | One,Opt -> 1
    | One,One -> 0

  let (<) a b = compare a b < 0
  let (>) a b = compare a b > 0
  let (>=) a b = compare a b >= 0
  let (<=) a b = compare a b <= 0
  let (<>) a b = compare a b <> 0

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
    screenOutputCtyp : ct;
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

  let rec show ((_, syn) : t) = match syn with
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
  | Navigate (parent,{ navName; navArgs = _ }) ->
    let parent = show parent in
    let this = navName in
    {j|$parent.$this|j}
  | One _ -> "one"
  | First _ -> "first"
  | Chain (parent, this) ->
    let parent = show parent in
    let this = show this in
    {j|$parent.bind($this)|j}
  | Screen (parent, { screenName; screenArgs = _ }) ->
    let parent = show parent in
    {j|$parent.render($screenName)|j}

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

    val make : name : string -> typ : Type.t -> queryResult -> TypedQuery.t -> t
    val test : 'a -> bool

    val name : t -> string
    val query : t -> TypedQuery.t
    val value : t -> queryResult
    val typ : t -> Type.t

  end = struct
    type t = <
      name : string;
      query : TypedQuery.t;
      value : queryResult;
      typ : Type.t;
    > Js.t

    external make : name : string -> typ : Type.t -> queryResult -> TypedQuery.t -> t =
      "UIRepr" [@@bs.new] [@@bs.module "./UIRepr"]

    let test_ : 'a -> bool = [%bs.raw {|
      function test(v) { return v instanceof UIRepr.UIRepr; }
    |}]

    let test x = test_ (Obj.magic x)

    let name ui = ui##name
    let query ui = ui##query
    let value ui = ui##value
    let typ ui = ui##typ
  end

  let null : t = Obj.magic (Js.null)
  external string : string -> t = "%identity"
  external number : float -> t = "%identity"
  external bool : bool -> t = "%identity"
  external ui : UI.t -> t = "%identity"
  external array : t array -> t = "%identity"
  external obj : t Js.Dict.t -> t = "%identity"
  external ofJson : Js.Json.t -> t = "%identity"

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
    outputCtyp : Type.t -> Type.ct
  }

  and fieldResolver =
    screenArgs : Arg.t list
    -> args : Arg.t list
    -> Value.t
    -> (Value.t, string) Result.t

  let typ ~name ~typ screen =
    let fields = Belt.List.map (screen.fields typ) (fun (field, _) -> field)
    in Type.Screen { screenName = name; screenFields = fields; screenOutputCtyp = screen.outputCtyp typ }

  let lookupField ~name ~typ screen =
    Belt.List.getBy (screen.fields typ) (fun (field, _) -> field.Type.fieldName = name)

  module Syntax = struct

    include Type.Syntax.Value

    let screen ?args ~inputCard ~outputCtyp fields = {
      fields;
      args;
      inputCard;
      outputCtyp;
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

end = struct

  let rec extractField univ fieldName (typ : Type.t) =
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
        let navigation = { TypedQuery. navName; navArgs; } in
        let%bind parent = aux ~ctyp parent in
        let (parentCard, parentTyp), _parentSyn = parent in
        let%bind field = extractField univ navName parentTyp in
        let fieldCard, fieldTyp = field.fieldCtyp in
        let fieldCard = Card.merge parentCard fieldCard in
        return ((fieldCard, fieldTyp), TypedQuery.Navigate (parent, navigation))
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

  val runQuery : t -> TypedQuery.t -> (Value.t, string) Result.t

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

  let runQuery db query =
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
      | TypedQuery.Screen (q, { screenName; screenArgs = _; }) ->
        return (Value.ui (Value.UI.make ~name:screenName ~typ value q))
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
          let%bind screen = Result.ofOption
            ~err:{j|no such screen "$screenName"|j}
            (Universe.lookupScreen screenName db.univ)
          in
          let%bind _, resolve = Result.ofOption
            ~err:{j|no such field "$navName"|j}
            (Screen.lookupField ~name:navName ~typ:(Value.UI.typ ui) screen)
          in resolve ~screenArgs:[] ~args value
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
    in aux ~value:db.root query

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
    type t = TypedQuery.t
  end)
end

module WorkflowTyper = struct

  let rootCtyp = Card.One, Type.Void

  let typeWorkflow ~univ w =
    let open Result.Syntax in
    let rec aux ~ctyp w =
      match w with
      | UntypedWorkflow.Render q ->
        let%bind ((_, typ), _) as q = QueryTyper.typeQuery ~univ ~ctyp q in
        begin match typ with
        | Type.Void | Type.Entity _ | Type.Record _ | Type.Value _ ->
          let typ = Type.show typ in
          let msg = {j|workflow can only be defined with screen but got $typ|j} in
          error msg
        | Type.Screen { screenOutputCtyp; _ } ->
          return (TypedWorkflow.Render q, screenOutputCtyp)
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

module WorkflowRunner (Db : DATABASE) : sig

  (**
   * This type represents workflow execution state.
   *)
  type t

  (**
   * Produce an initial state for the given database and workflow description.
   *)
  val make : Universe.t -> Db.t -> TypedWorkflow.t -> t

  (**
   * Bind workflow execution state for the new query fragment.
   * TODO: We should instead define per UI operations.
   *)
  val bind : UntypedQuery.t -> t -> ((t * Value.UI.t), string) Result.t

  (**
   * Render workflows state and return a new state and a UI screen to render.
   *)
  val renderState : t -> ((t * Value.UI.t), string) Result.t

  (**
   * Return a list of next possible workflow states.
   *)
  val next : t -> t list

  val query : Value.UI.t -> t -> TypedQuery.t

end = struct

  type t = {
    query : TypedQuery.t;
    queryString : string;
    workflow : TypedWorkflow.t;
    db : Db.t;
    univ : Universe.t;
    parent : t option;
  }

  let _make ?parent ?(query=TypedQuery.void) univ db workflow = {
    univ;
    query;
    queryString = TypedQuery.show query;
    db;
    workflow;
    parent;
  }

  let make db workflow = _make db workflow

  let rec renderState state =
    let open Result.Syntax in
    let {workflow; db; query; univ; _} = state in
    match workflow with
    | TypedWorkflow.Next (first, _next) ->
      let state = _make ~parent:state ~query univ db first in
      renderState state
    | TypedWorkflow.Render q ->
      let ctyp, _ = q in
      let q = ctyp, TypedQuery.Chain (query, q) in
      let state = { state with query = q; queryString = TypedQuery.show q; parent = Some state; } in
      let%bind res = Db.runQuery db q in
      match Value.classify res with
      | Value.UI ui -> return (state, ui)
      | _ -> error "expected UI, got data"

  let next state =
    let rec aux query state =
      let {workflow; db; univ; parent} = state in
      match workflow, parent with
      | TypedWorkflow.Render _, Some parent -> aux query parent
      | TypedWorkflow.Render _, None -> []
      | TypedWorkflow.Next (_first, []), Some parent -> aux query parent
      | TypedWorkflow.Next (_first, next), _ ->
        let f w = _make ~query ~parent:state univ db w in
        List.map f next
    in aux state.query state

  let bind q state =
    let open Result.Syntax in
    let ctyp, _ = state.query in
    let%bind (ctyp, _) as q = QueryTyper.typeQuery ~univ:state.univ ~ctyp q in
    let q = ctyp, TypedQuery.Chain (state.query, q) in
    let nextState = { state with query = q; queryString = TypedQuery.show q; } in
    match next nextState with
    | [] -> renderState state
    | nextState::_ -> renderState nextState

  let query ui state =
    let baseQuery = match state.parent with
    | Some parent -> parent.query
    | None -> (Card.One, Type.Void), TypedQuery.Void
    in
    let (ctyp, _) as q = Value.UI.query ui in
    let q = ctyp, TypedQuery.Chain (baseQuery, q) in
    q

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

  val start : state JsResult.t
  val renderState : state -> < state : state; ui : ui > Js.t JsResult.t
  val next : state -> state list
  val bind : uquery -> state -> < state : state; ui : ui > Js.t JsResult.t

  val pickValue : float -> uquery

  val uiName : ui -> string
  val getQuery : ui -> state -> query
  val runQuery : query -> Value.t JsResult.t

  val db : JSONDatabase.t
  val univ : Universe.t

  val showQuery : TypedQuery.t -> string

end = struct
  module WorkflowRunner = WorkflowRunner(JSONDatabase)

  type ui = Value.UI.t
  type state = WorkflowRunner.t
  type query = TypedQuery.t
  type uquery = UntypedQuery.t

  let showQuery = TypedQuery.show

  let uiName = Value.UI.name

  let univ =

    let site = Type.Syntax.(entity "site" [
      hasOne "title" string;
    ]) in

    let individual = Type.Syntax.(entity "individual" [
      hasOne "name" string;
      hasOne "site" site;
    ]) in

    let pickScreen =
      let resolveTitle ~screenArgs:_ ~args:_ _value =
        Result.Ok (Value.string "Pick Screen")
      in
      let resolveValue ~screenArgs:_ ~args:_ value =
        match Value.classify value with
        | Value.Array value ->
          Result.Ok (
            if Array.length value = 0
            then Value.null
            else Array.get value 0)
        | _ -> Result.Error "expected an array"
      in
      Screen.Syntax.(screen
        ~inputCard:Card.Many
        ~outputCtyp:(fun entity -> (Card.One, entity))
        (fun entity -> [
          has ~resolve:resolveTitle "title" string;
          has ~resolve:resolveValue ~card:Card.Opt "value" entity;
        ])
      )
    in

    let viewScreen =
      let resolveTitle ~screenArgs:_ ~args:_ _value =
        Result.Ok (Value.string "View Screen")
      in
      let resolveValue ~screenArgs:_ ~args:_ value =
        Result.Ok value
      in
      Screen.Syntax.(screen
        ~inputCard:Card.One
        ~outputCtyp:(fun entity -> (Card.Opt, entity))
        (fun entity -> [
          has ~resolve:resolveTitle "title" string;
          has ~resolve:resolveValue ~card:Card.Opt "value" entity;
        ])
      )
    in


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
      let state = WorkflowRunner.make univ db w in
      return state
    in JsResult.ofResult v

  let next = WorkflowRunner.next

  let renderState state = JsResult.ofResult (
    let open Result.Syntax in
    let%bind state, ui = WorkflowRunner.renderState state in
    return [%bs.obj { state; ui }]
  )

  let bind q state = JsResult.ofResult (
    let open Result.Syntax in
    let%bind state, ui = WorkflowRunner.bind q state in
    return [%bs.obj { state; ui }]
  )

  let pickValue id =
    UntypedQuery.Syntax.(
      here |> nav ~args:[Arg.number "id" id] "value"
    )

  let getQuery ui state = WorkflowRunner.query ui state
  let runQuery q = JsResult.ofResult (JSONDatabase.runQuery db q)
end

include JsApi

module Test = struct

  let univ = JsApi.univ
  let db = JsApi.db

  (**
    * {
    *   individuals: individual
    *   individualsNames: individual.name
    *   sites: individual.site
    *   siteTitles: individual.site.title
    * }
    *)
  let getSomeData = UntypedQuery.Syntax.(
    void
    |> select [
      field ~alias:"individuals" (here |> nav "individual");
      field ~alias:"individualNames" (here |> nav "individual" |> nav "name");
      field ~alias:"sites" (here |> nav "individual" |> nav "site");
      field ~alias:"siteTitles" (here |> nav "individual" |> nav "site" |> nav "title");
    ]
  )

  (*
   * individual.site.pick
   *)
  let renderListOfSites = UntypedQuery.Syntax.(
    void |> nav "individual" |> nav "site" |> screen "pick"
  )

  (*
   * individual.site.first.view
   *)
  let renderFirstSite = UntypedQuery.Syntax.(
    void |> nav "individual" |> nav "site" |> first |> screen "view"
  )

  (*
   * individual.pick.value(id: "someid").site.view
   *)
  let renderSiteByIndividual = UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> screen "pick"
    |> nav ~args:[Arg.number "id" 1.] "value"
    |> nav "site"
    |> screen "view"
  )

  (*
   * individual.pick.value(id: "someid").site.view
   *)
  let getSiteTitleByIndividualViaView = UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> screen "pick"
    |> nav ~args:[Arg.number "id" 1.] "value"
    |> nav "site"
    |> screen "view"
    |> nav "value"
    |> nav "title"
  )

  (*
   * individual.pick.value(id: "someid").site.view
   *)
  let getSiteTitleByIndividualViaViewViaBind2 = UntypedQuery.Syntax.(
    void
    |> chain (
      here
      |> nav "individual"
      |> screen "pick"
      |> chain (
        here
        |> nav ~args:[Arg.number "id" 1.] "value"
        |> chain (
          here
          |> nav "site"
          |> screen "view"
        )
      )
    )
  )

  (*
   * individual.pick.value(id: "someid").site.view
   *)
  let getSiteTitleByIndividualViaViewViaBind = UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> screen "pick"
    |> chain (
      here
      |> nav ~args:[Arg.number "id" 1.] "value"
      |> nav "site"
      |> screen "view"
      |> nav "value"
      |> chain (
        here
        |> nav "title"
      )
    )
  )

  (*
   * individual.pick.value(id: "someid").site.view
   *)
  let getSelectedIndividual = UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> screen "pick"
    |> nav ~args:[Arg.number "id" 1.] "value"
  )

  let pickAndViewIndividualWorkflow =
    let open UntypedWorkflow.Syntax in
    let open UntypedQuery.Syntax in

    let pickIndividual = render (here |> nav "individual" |> screen "pick") in

    let view = render (here |> screen "view") in

    let viewSite = render (here |> nav "site" |> screen "view") in

    pickIndividual |> andThen [ view; viewSite; ]

  let runResult result = match result with
    | Result.Ok () -> ()
    | Result.Error err -> Js.log2 "ERROR:" err

  let runQuery db query =
    Js.log "--- RUNNING QUERY ---";
    let result =
      Js.log2 "QUERY:" (UntypedQuery.show query);
      let open Result.Syntax in
      Js.log "TYPING...";
      let%bind query = QueryTyper.typeQuery ~univ query in
      let (_, typ), _ = query in
      Js.log2 "TYPE:" (Type.show typ);
      Js.log "RUNNING...";
      let%bind result = JSONDatabase.runQuery db query in
      Js.log2 "RESULT:" result;
      return ()
    in
    runResult result;
    Js.log "--- DONE ---"

  let typeWorkflow w =
    Js.log "TYPING WORKFLOW...";
    runResult (Result.ignore (WorkflowTyper.typeWorkflow ~univ w))

  let () =
    Js.log db;
    runQuery db getSomeData;
    runQuery db renderListOfSites;
    runQuery db renderFirstSite;
    runQuery db renderSiteByIndividual;
    runQuery db getSelectedIndividual;
    runQuery db getSiteTitleByIndividualViaView;
    runQuery db getSiteTitleByIndividualViaViewViaBind;
    runQuery db getSiteTitleByIndividualViaViewViaBind2;
    runQuery db UntypedQuery.Syntax.(void |> nav "individual" |> screen "pick" |> nav "title");
    typeWorkflow pickAndViewIndividualWorkflow;

end

