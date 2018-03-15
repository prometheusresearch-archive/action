(**
 * Rabbit based UI query language.
 *)

module Result = struct
  include Js.Result

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

module Cardinality = struct
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

module Value = struct
  type t =
    | String of string
    | Number of float
    | Bool of bool

  let show = function
    | String v -> v
    | Number v -> string_of_float v
    | Bool v -> string_of_bool v
end

module Type = struct

  type t =
    | Void
    | PickUI of t
    | ViewUI of t
    | Entity of (string * field list option)
    | Record of field list
    | Value of (ValueType.t * field list option)

  and arg = {
    argName : string;
    argTyp : Value.t;
    argKind : argKind;
  }

  and argKind =
    | Req
    | Opt

  and field = {
    fieldName : string;
    fieldArgs : arg list option;
    fieldTyp : t;
    fieldCard : Cardinality.t;
  }

  let entity name fields = Entity (name, Some fields)

  let has ?(card=Cardinality.One) ?args name typ =
    {
      fieldName = name;
      fieldArgs = args;
      fieldTyp = typ;
      fieldCard = card;
    }

  let hasOne = has ~card:Cardinality.One
  let hasOpt = has ~card:Cardinality.Opt
  let hasMany = has ~card:Cardinality.Many

  let string = Value (ValueType.StringTyp, None)
  let number = Value (ValueType.NumberTyp, None)
  let bool = Value (ValueType.BoolTyp, None)

  let rec show = function
    | Void -> "void"
    | Value (t, _fields) -> ValueType.show t
    | PickUI t -> let t = show t in {j|PickUI($t)|j}
    | ViewUI t -> let t = show t in {j|ViewUI($t)|j}
    | Entity (name, _fields) -> {j|Entity($name)|j}
    | Record _fields -> "{}"
end

(**
 * Universe is a collection of fields which are available on a void type.
 *)
module Universe : sig

  type t

  (**
   * An empty universe.
   *)
  val empty : t

  val hasOne : ?args : Type.arg list -> string -> Type.t -> t -> t
  val hasOpt : ?args : Type.arg list -> string -> Type.t -> t -> t
  val hasMany : ?args : Type.arg list -> string -> Type.t -> t -> t

  val fields : t -> Type.field list

end = struct

  module Map = Belt.Map.String

  type t = Type.field list

  let empty = []

  let hasOne ?args name typ univ =
    let field = Type.hasOne ?args name typ in
    field::univ

  let hasOpt ?args name typ univ =
    let field = Type.hasOpt ?args name typ in
    field::univ

  let hasMany ?args name typ univ =
    let field = Type.hasMany ?args name typ in
    field::univ

  let fields univ = univ
end

module Arg = struct

  type t = {
    name : string;
    value : Value.t
  }

  let make name value = { name; value }

end

module Query (V : sig type t end)= struct

  type t = V.t * syntax

  and syntax =
    | Void
    | Here
    | Select of (t * select)
    | Navigate of t * nav
    | One of t
    | First of t
    | PickUI of t
    | ViewUI of t

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

module UntypedQuery = struct
  include Query(struct type t = unit end)

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

    let renderPick query =
      (), PickUI query

    let renderView query =
      (), ViewUI query

    let one query =
      (), One query

    let first query =
      (), First query
  end

end

module TypedQuery = struct
  include Query(struct
    type t = (Cardinality.t * Type.t)
  end)
end

module Typer : sig

  val typeQuery : Universe.t -> UntypedQuery.t -> (TypedQuery.t, string) Result.t

end = struct

  let extractField univ fieldName (typ : Type.t) =
    let open Result.Syntax in
    let findInFieldList fields =
      match Belt.List.getBy fields (fun field -> field.Type.fieldName = fieldName) with
      | None -> error "no such field"
      | Some field -> Ok field
    in
    match typ with
    | Type.Void -> let fields = Universe.fields univ in findInFieldList fields
    | Type.PickUI typ ->
      begin match fieldName with
      | "value" -> Ok {
          Type.
          fieldName = "value";
          fieldArgs = None;
          fieldTyp = typ;
          fieldCard = Cardinality.Opt
        }
      | _ -> error {j|no such field on PickUI: $fieldName|j}
      end
    | Type.ViewUI typ ->
      begin match fieldName with
      | "value" -> Ok {
          Type.
          fieldName = "value";
          fieldArgs = None;
          fieldTyp = typ;
          fieldCard = Cardinality.Opt
        }
      | _ -> error {j|no such field on ViewUI: $fieldName|j}
      end
    | Type.Entity (_, None) -> error "cannot extract field"
    | Type.Entity (_, Some fields) -> findInFieldList fields
    | Type.Record fields -> findInFieldList fields
    | Type.Value _ -> error "cannot extract field"

  let typeQuery univ query =
    let rec aux ~scope ((), query) =
      let open Result.Syntax in
      match query with
      | UntypedQuery.Void ->
        return ((Cardinality.One, Type.Void), TypedQuery.Void)
      | UntypedQuery.Here ->
        return (scope, TypedQuery.Here)
      | UntypedQuery.One parent ->
        let%bind parent = aux ~scope parent in
        return (scope, TypedQuery.One parent)
      | UntypedQuery.First parent ->
        let%bind ((_, parentType), _) as parent = aux ~scope parent in
        return ((Cardinality.Opt, parentType), TypedQuery.One parent)
      | UntypedQuery.PickUI parent ->
        let%bind ((parentCard, parentType), _) as parent = aux ~scope parent in
        begin match parentCard with
        | Cardinality.One
        | Cardinality.Opt -> error "pick can only be rendered with queries which result in a list of entities"
        | Cardinality.Many ->
          return ((Cardinality.One, Type.PickUI parentType), TypedQuery.PickUI parent)
        end
      | UntypedQuery.ViewUI parent ->
        let%bind ((parentCard, parentType), _) as parent = aux ~scope parent in
        Js.log (Type.show parentType);
        begin match parentCard with
        | Cardinality.One
        | Cardinality.Opt ->
          return ((Cardinality.One, Type.ViewUI parentType), TypedQuery.ViewUI parent)
        | Cardinality.Many ->
          error "view can only be rendered with queries which result in nothing or a single entity"
        end
      | UntypedQuery.Navigate (parent, navigation) ->
        let { UntypedQuery. name; args } = navigation in
        let navigation = { TypedQuery. name; args; } in
        let%bind parent = aux ~scope parent in
        let (parentCard, parentTyp), _parentSyn = parent in
        let%bind field = extractField univ name parentTyp in
        let card = Cardinality.merge parentCard field.fieldCard in
        return ((card, field.fieldTyp), TypedQuery.Navigate (parent, navigation))
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
            let fieldCard = Cardinality.merge parentCard fieldCard in
            let field = { Type. fieldTyp; fieldCard; fieldName; fieldArgs = None } in
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
    in aux ~scope:(Cardinality.One, Type.Void) query

end

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
      | TypedQuery.PickUI q -> return (QueryResult.ui (UI.make "pick" typ q))
      | TypedQuery.ViewUI q -> return (QueryResult.ui (UI.make "view" typ q))
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
          | Type.PickUI _, QueryResult.Array items -> return (Array.get items 0)
          | Type.ViewUI _, QueryResult.Object _ -> return parent
          | _ -> error "PickUI returns not an array"
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

module Test = struct

  let univ =
    let site = Type.(entity "site" [
      hasOne "title" string;
    ]) in
    let individual = Type.(entity "individual" [
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
    void |> nav "individual" |> nav "site" |> renderPick
  )

  (*
   * individual.site.first.view
   *)
  let renderFirstSite = UntypedQuery.Syntax.(
    void |> nav "individual" |> nav "site" |> first |> renderView
  )

  (*
   * individual.pick.value(id: "someid").site.view
   *)
  let renderSiteByIndividual = UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> renderPick
    |> nav ~args:[Arg.make "id" (Value.Number 1.)] "value"
    |> nav "site"
    |> renderView
  )

  (*
   * individual.pick.value(id: "someid")
   *)
  let getSelectedIndividual = UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> renderPick
    |> nav ~args:[Arg.make "id" (Value.Number 1.)] "value"
  )

  (*
   * individual.pick.value(id: "someid").site.view
   *)
  let getSiteTitleByIndividualViaView = UntypedQuery.Syntax.(
    void
    |> nav "individual"
    |> renderPick
    |> nav ~args:[Arg.make "id" (Value.Number 1.)] "value"
    |> nav "site"
    |> renderView
    |> nav "value"
    |> nav "title"
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

  let runQuery db query =
    Js.log "--- RUNNING QUERY ---";
    let result =
      let open Result.Syntax in
      Js.log "TYPING...";
      let%bind query = Typer.typeQuery univ query in
      let (_, typ), _ = query in
      Js.log2 "TYPE:" (Type.show typ);
      Js.log "RUNNING...";
      let%bind result = JSONDatabase.runQuery db query in
      Js.log2 "RESULT:" result;
      return ()
    in
    match result with
    | Ok () -> ()
    | Error err -> Js.log2 "ERROR:" err;
    Js.log "--- DONE ---"

  let () =
    Js.log db;
    runQuery db getSomeData;
    runQuery db renderListOfSites;
    runQuery db renderFirstSite;
    runQuery db renderSiteByIndividual;
    runQuery db getSelectedIndividual;
    runQuery db getSiteTitleByIndividualViaView;

end
