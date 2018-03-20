(**
 * Rabbit based UI query language.
 *)

module Result = struct
  include Js.Result

  let ignore = function
    | Ok _ -> Ok ()
    | Error err -> Error err

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
    | UI of ui
    | Entity of entityInfo
    | Record of field list
    | Value of (ValueType.t * field list option)

  and ui =
    | PickScreen of entityInfo
    | ViewScreen of entityInfo

  and entityInfo = {
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

  let extractUi = function
    | UI ui -> Result.Ok ui
    | _ -> Result.Error "not an ui type"

  let rec show = function
    | Void -> "void"
    | Value (t, _fields) -> ValueType.show t
    | UI (PickScreen entityInfo) -> let t = show (Entity entityInfo) in {j|PickScreen($t)|j}
    | UI (ViewScreen entityInfo) -> let t = show (Entity entityInfo) in {j|ViewScreen($t)|j}
    | Entity {entityName; _} -> {j|Entity($entityName)|j}
    | Record _fields -> "{}"

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

    let string = Value (ValueType.StringTyp, None)
    let number = Value (ValueType.NumberTyp, None)
    let bool = Value (ValueType.BoolTyp, None)
  end

end

(**
 * Type annotated with cardinality.
 *)
module CType = struct
  type t = Card.t * Type.t
end

(**
 * Universe is a collection of types which are available on a void type.
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

  val fields : t -> Type.field list

end = struct

  module Map = Belt.Map.String

  type t = Type.field list

  let empty = []

  let hasOne ?args name typ univ =
    let field = Type.Syntax.hasOne ?args name typ in
    field::univ

  let hasOpt ?args name typ univ =
    let field = Type.Syntax.hasOpt ?args name typ in
    field::univ

  let hasMany ?args name typ univ =
    let field = Type.Syntax.hasMany ?args name typ in
    field::univ

  let fields univ = univ
end

(**
 * This defines a query syntax parametrized by the payload.
 *
 * Payload can be used to store some semantic info along with a query, for
 * example location of the query sources parsed from source files or type
 * information.
 *)
module Query (P : sig type t end)= struct

  type payload = P.t

  type t = payload * syntax

  and syntax =
    | Void
    | Here
    | Select of (t * select)
    | Navigate of t * nav
    | One of t
    | First of t
    | Bind of (t * t)
    | PickScreen of t
    | ViewScreen of t

  and nav = {
    name : string;
    args : Arg.t list option;
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
    {j|$parent { $fields }|j}
  | Navigate (parent,{ name; args = _ }) ->
    let parent = show parent in
    let this = name in
    {j|$parent.$this|j}
  | One _ -> "one"
  | First _ -> "first"
  | Bind (parent, this) ->
    let parent = show parent in
    let this = show this in
    {j|$parent.bind($this)|j}
  | PickScreen parent ->
    let parent = show parent in
    {j|$parent.pick|j}
  | ViewScreen parent ->
    let parent = show parent in
    {j|$parent.view|j}

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
      (), Navigate (parent, { name; args; })

    let bind q parent =
      (), Bind (parent, q)

    let select fields parent =
      (), Select (parent, fields)

    let field ?alias query =
      { query; alias; }

    let pickScreen query =
      (), PickScreen query

    let viewScreen query =
      (), ViewScreen query

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
    type t = CType.t
  end)

  let void = (Card.One, Type.Void), Void
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

  let extractField univ fieldName (typ : Type.t) =
    let open Result.Syntax in
    let findInFieldList fields =
      match Belt.List.getBy fields (fun field -> field.Type.fieldName = fieldName) with
      | None -> error {j|no such field: $fieldName|j}
      | Some field -> Ok field
    in
    match typ with
    | Type.Void -> let fields = Universe.fields univ in findInFieldList fields
    | Type.UI (Type.PickScreen entityInfo) ->
      begin match fieldName with
      | "value" -> Ok {
          Type.
          fieldName = "value";
          fieldArgs = None;
          fieldCtyp = Card.Opt, Type.Entity entityInfo;
        }
      | _ -> error {j|no such field on PickScreen: $fieldName|j}
      end
    | Type.UI (Type.ViewScreen entityInfo) ->
      begin match fieldName with
      | "value" -> Ok {
          Type.
          fieldName = "value";
          fieldArgs = None;
          fieldCtyp = Card.Opt, Type.Entity entityInfo;
        }
      | _ -> error {j|no such field on ViewScreen: $fieldName|j}
      end
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
      | UntypedQuery.Bind (parent, q) ->
        let%bind ((prevCard, _) as ctyp, _) as parent = aux ~ctyp parent in
        let%bind ((card, typ), _) as q = aux ~ctyp q in
        let ctyp = Card.merge prevCard card, typ in
        return (ctyp, TypedQuery.Bind (parent, q))
      | UntypedQuery.One parent ->
        let%bind parent = aux ~ctyp parent in
        return (ctyp, TypedQuery.One parent)
      | UntypedQuery.First parent ->
        let%bind ((_, parentType), _) as parent = aux ~ctyp parent in
        return ((Card.Opt, parentType), TypedQuery.One parent)
      | UntypedQuery.PickScreen parent ->
        let%bind ((parentCard, parentTyp), _) as parent = aux ~ctyp parent in
        begin match parentCard, parentTyp with
        | Card.One, Type.Entity _
        | Card.Opt, Type.Entity _ ->
          error "pick can only be rendered with queries which result in a list of entities"
        | Card.Many, Type.Entity entityInfo ->
          return ((Card.One, Type.UI (Type.PickScreen entityInfo)), TypedQuery.PickScreen parent)
        | _, _ -> error "pick can only be applied to entity type"
        end
      | UntypedQuery.ViewScreen parent ->
        let%bind ((parentCard, parentTyp), _) as parent = aux ~ctyp parent in
        begin match parentCard, parentTyp with
        | Card.One, Type.Entity entityInfo
        | Card.Opt, Type.Entity entityInfo ->
          return ((Card.One, Type.UI (Type.ViewScreen entityInfo)), TypedQuery.ViewScreen parent)
        | Card.Many, Type.Entity _ ->
          error "view can only be rendered with queries which result in nothing or a single entity"
        | _, _ ->
          error "view can only be rendered with queries which result entity"
        end
      | UntypedQuery.Navigate (parent, navigation) ->
        let { UntypedQuery. name; args } = navigation in
        let navigation = { TypedQuery. name; args; } in
        let%bind parent = aux ~ctyp parent in
        let (parentCard, parentTyp), _parentSyn = parent in
        let%bind field = extractField univ name parentTyp in
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
 * Query result which extends JSON type with a special UI type.
 *
 * It is implemented as a zero (almost) cost on top of native JS data
 * structures.
 *)
module QueryResult = struct

  type t
  type queryResult = t

  (**
  * This is an opaque structure which defines UI.
  *)
  module UI : sig

    type t

    val make : name : string -> uiTyp : Type.ui -> queryResult -> TypedQuery.t -> t
    val test : 'a -> bool
    val query : t -> TypedQuery.t
    val value : t -> queryResult
    val typ : t -> Type.ui

  end = struct
    type t = <
      name : string;
      typ: Type.ui;
      query : TypedQuery.t;
      value : queryResult;
    > Js.t

    external make : name : string -> uiTyp : Type.ui -> queryResult -> TypedQuery.t -> t =
      "UIRepr" [@@bs.new] [@@bs.module "./UIRepr"]

    let test_ : 'a -> bool = [%bs.raw {|
      function test(v) { return v instanceof UIRepr.UIRepr; }
    |}]

    let test x = test_ (Obj.magic x)

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

(**
 * Abstract interface to the database.
 *)
module type DATABASE = sig

  type t

  val runQuery : t -> TypedQuery.t -> (QueryResult.t, string) Result.t

end

(**
 * A database for an in-memory JSON objects.
 *)
module JSONDatabase : sig
  include DATABASE

  val ofString : string -> t
  val ofJson : Js.Json.t -> t
end = struct

  type t = Js.Json.t

  let ofString = Js.Json.parseExn
  let ofJson dataset = dataset

  let runQuery db query =
    let open Result.Syntax in
    let root = QueryResult.ofJson db in
    let rec aux ~(value : QueryResult.t) ((_card, typ), syn) =
      match syn with
      | TypedQuery.Void -> return root
      | TypedQuery.Here ->
        return value
      | TypedQuery.Bind (query, next) ->
        let%bind value = aux ~value query in
        let%bind value = aux ~value next in
        return value
      | TypedQuery.One query ->
        let%bind value = aux ~value query in
        begin match QueryResult.classify value with
        | QueryResult.Array items ->
          if Array.length items = 1
          then return (Array.get items 0)
          else error "expected a single value but got multiple"
        | QueryResult.Null -> error "expected a single value but got null"
        | _ -> return value
        end
      | TypedQuery.First query ->
        let%bind value = aux ~value query in
        begin match QueryResult.classify value with
        | QueryResult.Array items ->
          if Array.length items = 1
          then return (Array.get items 0)
          else return QueryResult.null
        | _ -> return value
        end
      | TypedQuery.PickScreen q ->
        let%bind uiTyp = Type.extractUi typ in
        return (QueryResult.ui (QueryResult.UI.make ~name:"pick" ~uiTyp value q))
      | TypedQuery.ViewScreen q ->
        let%bind uiTyp = Type.extractUi typ in
        return (QueryResult.ui (QueryResult.UI.make ~name:"view" ~uiTyp value q))
      | TypedQuery.Navigate (query, { name; args }) ->
        let%bind value = aux ~value query in
        let navigate name dataset =
          match QueryResult.classify dataset with
          | QueryResult.Object obj ->
            begin match Js.Dict.get obj name with
            | Some dataset -> return dataset
            | None ->
              let msg = {j|no such key "$name"|j} in
              Js.log3 "ERROR:" msg [%bs.obj { data = dataset; key = name; }];
              error msg
            end
          | _ -> error "expected an object"
        in begin
        match QueryResult.classify value, name with
        | QueryResult.Object _, name -> navigate name value
        | QueryResult.Array items, name  ->
          let%bind items = Result.Array.map ~f:(navigate name) items in
          return (QueryResult.array items)
        | QueryResult.UI ui, name ->
          let query = QueryResult.UI.query ui in
          let queryValue = QueryResult.UI.value ui in
          let%bind value = aux ~value:queryValue query in
          begin match (QueryResult.UI.typ ui), name, args, QueryResult.classify value with
          | Type.PickScreen _, "value", None, _ ->
            error "missing id argument"
          | Type.PickScreen _, "value", Some args, QueryResult.Array items ->
            let value: QueryResult.t = match Arg.findValueFromArgList ~name:"id" args with
            | Some (Arg.Bool v) -> Obj.magic v
            | Some (Arg.String v) -> Obj.magic v
            | Some (Arg.Number v) -> Obj.magic v
            | None -> Obj.magic Js.Nullable.null
            in
            let f item = match QueryResult.classify item with
              | QueryResult.Object obj ->
                let id = Option.getWithDefault QueryResult.null (Js.Dict.get obj "id") in
                id = (Obj.magic value)
              | _ -> false
            in
            return (match Js.Array.find f items with
              | Some v -> v
              | None -> QueryResult.null
            )
          | Type.ViewScreen _, "value", _, QueryResult.Object _ ->
            return value
          | _, name, _, _ -> error {j|no such key "$name"|j}
          end
        | _ -> error "expected an object or an array"
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
        in return (QueryResult.obj dataset)
    in aux ~value:root query

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
      match w with | UntypedWorkflow.Render q ->
        let%bind ((_, typ), _) as q = QueryTyper.typeQuery ~univ ~ctyp q in
        begin match typ with
        | Type.Void | Type.Entity _ | Type.Record _ | Type.Value _ ->
          error "workflow can only be defined on UI values"
        | Type.UI (Type.PickScreen entityInfo) ->
          let scope = Card.One, Type.Entity entityInfo in
          return (TypedWorkflow.Render q, scope)
        | Type.UI (Type.ViewScreen entityInfo) ->
          let scope = Card.One, Type.Entity entityInfo in
          return (TypedWorkflow.Render q, scope)
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
  val bind : UntypedQuery.t -> t -> ((t * QueryResult.UI.t), string) Result.t

  (**
   * Render workflows state and return a new state and a UI screen to render.
   *)
  val render : t -> ((t * QueryResult.UI.t), string) Result.t

  (**
   * Return a list of next possible workflow states.
   *)
  val next : t -> t list

  val query :QueryResult.UI.t -> t -> TypedQuery.t

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

  let rec render state =
    let open Result.Syntax in
    let {workflow; db; query; univ; _} = state in
    match workflow with
    | TypedWorkflow.Next (first, _next) ->
      let state = _make ~parent:state ~query univ db first in
      render state
    | TypedWorkflow.Render q ->
      let ctyp, _ = q in
      let q = ctyp, TypedQuery.Bind (query, q) in
      let state = { state with query = q; queryString = TypedQuery.show q; parent = Some state; } in
      let%bind res = Db.runQuery db q in
      match QueryResult.classify res with
      | QueryResult.UI ui -> return (state, ui)
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
    let q = ctyp, TypedQuery.Bind (state.query, q) in
    let nextState = { state with query = q; queryString = TypedQuery.show q; } in
    match next nextState with
    | [] -> render state
    | nextState::_ -> render nextState

  let query ui state =
    let baseQuery = match state.parent with
    | Some parent -> parent.query
    | None -> (Card.One, Type.Void), TypedQuery.Void
    in
    let (ctyp, _) as q = QueryResult.UI.query ui in
    let q = ctyp, TypedQuery.Bind (baseQuery, q) in
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
  val render : state -> < state : state; ui : ui > Js.t JsResult.t
  val next : state -> state list
  val bind : uquery -> state -> < state : state; ui : ui > Js.t JsResult.t

  val pickValue : float -> uquery

  val getQuery : ui -> state -> query
  val runQuery : query -> QueryResult.t JsResult.t

  val db : JSONDatabase.t
  val univ : Universe.t

  val showQuery : TypedQuery.t -> string

end = struct
  module WorkflowRunner = WorkflowRunner(JSONDatabase)

  type workflow = UntypedWorkflow.t
  type db = JSONDatabase.t
  type ui = QueryResult.UI.t
  type state = WorkflowRunner.t
  type query = TypedQuery.t
  type uquery = UntypedQuery.t

  let showQuery = TypedQuery.show

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
    )

  let db = JSONDatabase.ofString {|
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
            "title": "RexDB Site"
          }
        }
      ]
    }
  |}


  let workflow =
    let open UntypedWorkflow.Syntax in
    let open UntypedQuery.Syntax in

    let pickIndividual = render (here |> nav "individual" |> pickScreen) in

    let view = render (here |> viewScreen) in

    let viewSite = render (here |> nav "site" |> viewScreen) in

    pickIndividual |> andThen [ view; viewSite; ]

  let start =
    let v =
      let open Result.Syntax in
      let%bind w = WorkflowTyper.typeWorkflow ~univ workflow in
      let state = WorkflowRunner.make univ db w in
      return state
    in JsResult.ofResult v

  let next = WorkflowRunner.next

  let render state = JsResult.ofResult (
    let open Result.Syntax in
    let%bind state, ui = WorkflowRunner.render state in
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
    void |> nav "individual" |> nav "site" |> pickScreen
  )

  (*
   * individual.site.first.view
   *)
  let renderFirstSite = UntypedQuery.Syntax.(
    void |> nav "individual" |> nav "site" |> first |> viewScreen
  )

  (*
   * individual.pick.value(id: "someid").site.view
   *)
  let renderSiteByIndividual = UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> pickScreen
    |> nav ~args:[Arg.number "id" 1.] "value"
    |> nav "site"
    |> viewScreen
  )

  (*
   * individual.pick.value(id: "someid").site.view
   *)
  let getSiteTitleByIndividualViaView = UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> pickScreen
    |> nav ~args:[Arg.number "id" 1.] "value"
    |> nav "site"
    |> viewScreen
    |> nav "value"
    |> nav "title"
  )

  (*
   * individual.pick.value(id: "someid").site.view
   *)
  let getSiteTitleByIndividualViaViewViaBind2 = UntypedQuery.Syntax.(
    void
    |> bind (
      here
      |> nav "individual"
      |> pickScreen
      |> bind (
        here
        |> nav ~args:[Arg.number "id" 1.] "value"
        |> bind (
          here
          |> nav "site"
          |> viewScreen
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
    |> pickScreen
    |> bind (
      here
      |> nav ~args:[Arg.number "id" 1.] "value"
      |> nav "site"
      |> viewScreen
      |> nav "value"
      |> bind (
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
    |> pickScreen
    |> nav ~args:[Arg.number "id" 1.] "value"
  )

  let pickAndViewIndividualWorkflow =
    let open UntypedWorkflow.Syntax in
    let open UntypedQuery.Syntax in

    let pickIndividual = render (here |> nav "individual" |> pickScreen) in

    let view = render (here |> viewScreen) in

    let viewSite = render (here |> nav "site" |> viewScreen) in

    pickIndividual |> andThen [ view; viewSite; ]

  let runResult result = match result with
    | Result.Ok () -> ()
    | Result.Error err -> Js.log2 "ERROR:" err

  let runQuery db query =
    Js.log "--- RUNNING QUERY ---";
    let result =
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
    typeWorkflow pickAndViewIndividualWorkflow;

end

