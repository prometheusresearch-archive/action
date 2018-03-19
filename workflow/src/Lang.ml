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

end

(**
 * Type system.
 *)
module Type = struct

  type t =
    | Void
    | PickScreen of entityInfo
    | ViewScreen of entityInfo
    | Entity of entityInfo
    | Record of field list
    | Value of (ValueType.t * field list option)

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

  let rec show = function
    | Void -> "void"
    | Value (t, _fields) -> ValueType.show t
    | PickScreen entityInfo -> let t = show (Entity entityInfo) in {j|PickScreen($t)|j}
    | ViewScreen entityInfo -> let t = show (Entity entityInfo) in {j|ViewScreen($t)|j}
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
end

(**
 * This module implements a type checking / type inferrence for query structure
 * by turning untype queries into typed ones.
 *)
module QueryTyper : sig

  val typeQuery :
    ?scope : TypedQuery.payload
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
    | Type.PickScreen entityInfo ->
      begin match fieldName with
      | "value" -> Ok {
          Type.
          fieldName = "value";
          fieldArgs = None;
          fieldCtyp = Card.Opt, Type.Entity entityInfo;
        }
      | _ -> error {j|no such field on PickScreen: $fieldName|j}
      end
    | Type.ViewScreen entityInfo ->
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

  let rootScope = Card.One, Type.Void

  let typeQuery ?(scope=rootScope) ~univ query =
    let rec aux ~scope ((), query) =
      let open Result.Syntax in
      match query with
      | UntypedQuery.Void ->
        return ((Card.One, Type.Void), TypedQuery.Void)
      | UntypedQuery.Here ->
        return (scope, TypedQuery.Here)
      | UntypedQuery.One parent ->
        let%bind parent = aux ~scope parent in
        return (scope, TypedQuery.One parent)
      | UntypedQuery.First parent ->
        let%bind ((_, parentType), _) as parent = aux ~scope parent in
        return ((Card.Opt, parentType), TypedQuery.One parent)
      | UntypedQuery.PickScreen parent ->
        let%bind ((parentCard, parentTyp), _) as parent = aux ~scope parent in
        begin match parentCard, parentTyp with
        | Card.One, Type.Entity _
        | Card.Opt, Type.Entity _ ->
          error "pick can only be rendered with queries which result in a list of entities"
        | Card.Many, Type.Entity entityInfo ->
          return ((Card.One, Type.PickScreen entityInfo), TypedQuery.PickScreen parent)
        | _, _ -> error "pick can only be applied to entity type"
        end
      | UntypedQuery.ViewScreen parent ->
        let%bind ((parentCard, parentTyp), _) as parent = aux ~scope parent in
        begin match parentCard, parentTyp with
        | Card.One, Type.Entity entityInfo
        | Card.Opt, Type.Entity entityInfo ->
          return ((Card.One, Type.ViewScreen entityInfo), TypedQuery.ViewScreen parent)
        | Card.Many, Type.Entity _ ->
          error "view can only be rendered with queries which result in nothing or a single entity"
        | _, _ ->
          error "view can only be rendered with queries which result entity"
        end
      | UntypedQuery.Navigate (parent, navigation) ->
        let { UntypedQuery. name; args } = navigation in
        let navigation = { TypedQuery. name; args; } in
        let%bind parent = aux ~scope parent in
        let (parentCard, parentTyp), _parentSyn = parent in
        let%bind field = extractField univ name parentTyp in
        let fieldCard, fieldTyp = field.fieldCtyp in
        let fieldCard = Card.merge parentCard fieldCard in
        return ((fieldCard, fieldTyp), TypedQuery.Navigate (parent, navigation))
      | UntypedQuery.Select (parent, selection) ->
        let%bind parent = aux ~scope parent in
        let parentInfo, _parentSyn = parent in
        let parentCard, _parentTyp = parentInfo in
        let checkField fields { UntypedQuery. alias; query } =
          match fields with
          | Result.Ok (fields, selection, index) ->
            let%bind query = aux ~scope:parentInfo query in
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
    in aux ~scope query

end

(**
 * This is an opaque structure which defines UI.
 *)
module UI : sig

  type t

  val make : string -> Type.t -> TypedQuery.t -> t
  val test : 'a -> bool
  val query : t -> TypedQuery.t
  val typ : t -> Type.t

end = struct
  type t = < name : string; typ: Type.t; query : TypedQuery.t > Js.t

  external make : string -> Type.t -> TypedQuery.t -> t = "UIRepr" [@@bs.new] [@@bs.module "./UIRepr"]

  let test_ : 'a -> bool = [%bs.raw {|
    function test(v) { return v instanceof UIRepr.UIRepr; }
  |}]

  let test x = test_ (Obj.magic x)

  let query ui = ui##query
  let typ ui = ui##typ
end

(**
 * Query result which extends JSON type with a special UI type.
 *
 * It is implemented as a zero (almost) cost on top of native JS data
 * structures.
 *)
module QueryResult = struct

  type t

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
    let rec aux (parent : QueryResult.t) ((_card, typ), syn) =
      match syn with
      | TypedQuery.Void -> return parent
      | TypedQuery.Here -> return parent
      | TypedQuery.One query ->
        let%bind parent = aux parent query in
        begin match QueryResult.classify parent with
        | QueryResult.Array items ->
          if Array.length items = 1
          then return (Array.get items 0)
          else error "expected a single value but got multiple"
        | QueryResult.Null -> error "expected a single value but got null"
        | _ -> return parent
        end
      | TypedQuery.First query ->
        let%bind parent = aux parent query in
        begin match QueryResult.classify parent with
        | QueryResult.Array items ->
          if Array.length items = 1
          then return (Array.get items 0)
          else return QueryResult.null
        | _ -> return parent
        end
      | TypedQuery.PickScreen q -> return (QueryResult.ui (UI.make "pick" typ q))
      | TypedQuery.ViewScreen q -> return (QueryResult.ui (UI.make "view" typ q))
      | TypedQuery.Navigate (query, { name; args = _ }) ->
        let%bind parent = aux parent query in
        let navigate dataset =
          match QueryResult.classify dataset with
          | QueryResult.Object obj ->
            begin match Js.Dict.get obj name with
            | Some dataset -> return dataset
            | None -> error "no such key"
            end
          | _ -> error "expected an object"
        in begin
        match QueryResult.classify parent with
        | QueryResult.Object _ -> navigate parent
        | QueryResult.Array items ->
          let%bind items = Result.Array.map ~f:navigate items in
          return (QueryResult.array items)
        | QueryResult.UI ui ->
          let query = UI.query ui in
          let%bind parent = aux root query in
          begin match (UI.typ ui), QueryResult.classify parent with
          | Type.PickScreen _, QueryResult.Array items -> return (Array.get items 0)
          | Type.ViewScreen _, QueryResult.Object _ -> return parent
          | _ -> error "PickScreen returns not an array"
          end
        | _ -> error "expected an object or an array"
        end
      | TypedQuery.Select (query, selection) ->
        let%bind parent = aux parent query in
        let%bind _, dataset =
          let build state { TypedQuery. alias; query; } =
            match state with
            | Result.Ok (idx, dataset) ->
              let%bind selectionValue = aux parent query in
              let selectionAlias = Option.getWithDefault (string_of_int idx) alias in
              Js.Dict.set dataset selectionAlias selectionValue;
              return (idx + 1, dataset)
            | Result.Error err -> error err
          in
          Belt.List.reduce selection (Result.Ok (0, Js.Dict.empty ())) build
        in return (QueryResult.obj dataset)
    in aux root query

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
    let next path w = Next (w, path)
  end
end

module TypedWorkflow = struct
  include Workflow(struct
    type t = TypedQuery.t
  end)
end

module WorkflowTyper = struct

  let rootScope = Card.One, Type.Void

  let typeWorkflow ~univ w =
    let open Result.Syntax in
    let rec aux ~scope w =
      match w with | UntypedWorkflow.Render q ->
        let%bind ((_, typ), _) as q = QueryTyper.typeQuery ~univ ~scope q in
        begin match typ with
        | Type.Void | Type.Entity _ | Type.Record _ | Type.Value _ ->
          error "workflow can only be defined on UI values"
        | Type.PickScreen entityInfo ->
          let scope = Card.One, Type.Entity entityInfo in
          return (TypedWorkflow.Render q, scope)
        | Type.ViewScreen entityInfo ->
          let scope = Card.One, Type.Entity entityInfo in
          return (TypedWorkflow.Render q, scope)
        end
      | UntypedWorkflow.Next (first, next) ->
        let%bind first, scope = aux ~scope first in
        let%bind next, _ =
          let f (next, scope) w =
            let%bind w, _ = aux ~scope w in
            return (w::next, scope)
          in
          Result.List.foldLeft ~f ~init:([], scope) next
        in
        return (TypedWorkflow.Next (first, next), scope)
    in
    let%bind tw, _ = aux w ~scope:rootScope in
    return tw

end

module WorkflowRunnner (Db : DATABASE) : sig

  type t

  val make : ?parent : t -> Db.t -> TypedWorkflow.t -> t
  val render : t -> ((t * UI.t), string) Result.t
  val next : t -> t list

end = struct

  type t = {
    workflow : TypedWorkflow.t;
    db : Db.t;
    parent : t option;
  }

  let make ?parent db workflow = {db; workflow; parent}

  let rec render state =
    let open Result.Syntax in
    let {workflow; db; _} = state in
    match workflow with
    | TypedWorkflow.Next (first, _next) ->
      let state = make ~parent:state db first in
      render state
    | TypedWorkflow.Render q ->
      let%bind res = Db.runQuery db q in
      match QueryResult.classify res with
      | QueryResult.UI ui -> return (state, ui)
      | _ -> error "expected UI, got data"

  let rec next state =
    let {workflow; db; parent} = state in
    match workflow, parent with
    | TypedWorkflow.Render _, Some parent -> next parent
    | TypedWorkflow.Render _, None -> []
    | TypedWorkflow.Next (_first, []), Some parent -> next parent
    | TypedWorkflow.Next (_first, next), _ ->
      let f w = make ~parent:state db w in
      List.map f next

end

module Test = struct

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
  let getSelectedIndividual = UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> pickScreen
    |> nav ~args:[Arg.number "id" 1.] "value"
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
        }
      ]
    }
  |}

  let pickAndViewIndividualWorkflow =
    let open UntypedWorkflow.Syntax in
    let open UntypedQuery.Syntax in

    let pickIndividual = here |> nav "individual" |> pickScreen in

    let view = here |> viewScreen in

    let viewSite = here |> nav "site" |> viewScreen in

    render pickIndividual |> next [
      render view;
      render viewSite;
    ]

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
    typeWorkflow pickAndViewIndividualWorkflow;

end

module JsResult : sig
  type 'v t = <
    error : string Js.Nullable.t;
    result : 'v Js.Nullable.t;
  > Js.t

  val ofResult : ('v, string) Result.t -> 'v t

end = struct

  type 'v t = <
    error : string Js.Nullable.t;
    result : 'v Js.Nullable.t;
  > Js.t

  let ofResult = function
    | Result.Ok v -> [%bs.obj { result = Js.Nullable.return v; error = Js.Nullable.null }]
    | Result.Error error -> [%bs.obj { result = Js.Nullable.null; error = Js.Nullable.return error; }]

end

(**
 * JS API
 *)
module JsApi : sig

  type workflow
  type db
  type ui
  type state
  type query

  val db : db
  val workflow : workflow

  val init : < db : db; workflow : workflow > Js.t -> state JsResult.t
  val render : state -> (state * ui) JsResult.t
  val next : state -> state list

  val getQuery : ui -> query
  val runQuery : query -> QueryResult.t JsResult.t

end = struct
  module WorkflowRunnner = WorkflowRunnner(JSONDatabase)

  type workflow = UntypedWorkflow.t
  type db = JSONDatabase.t
  type ui = UI.t
  type state = WorkflowRunnner.t
  type query = TypedQuery.t

  let univ = Test.univ
  let db = Test.db

  let workflow =
    let open UntypedWorkflow.Syntax in
    let open UntypedQuery.Syntax in

    let pickIndividual = here |> nav "individual" |> pickScreen in

    let view = here |> viewScreen in

    let viewSite = here |> nav "site" |> viewScreen in

    render pickIndividual |> next [
      render view;
      render viewSite;
    ]

  let init params =
    let v =
      let open Result.Syntax in
      let%bind w = WorkflowTyper.typeWorkflow ~univ params##workflow in
      let state = WorkflowRunnner.make params##db w in
      return state
    in JsResult.ofResult v

  let next = WorkflowRunnner.next
  let render state = JsResult.ofResult (WorkflowRunnner.render state)

  let getQuery ui = UI.query ui
  let runQuery q = JsResult.ofResult (JSONDatabase.runQuery db q)
end
