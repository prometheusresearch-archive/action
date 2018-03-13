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
end

module ValueType = struct
  type t =
    | StringTyp
    | NumberTyp
    | BoolTyp
end

module Value = struct
  type t =
    | String of string
    | Number of float
    | Bool of bool
end

module Type = struct

  type t =
    | Void
    | UI of (string * field list option)
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
  let ui name fields = UI (name, Some fields)

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

end

module Query (V : sig type t end)= struct

  type t = V.t * syntax

  and syntax =
    | Void
    | Here
    | Select of (t * select)
    | Navigate of t * nav

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
    | Type.UI (_, None) -> error "cannot extract field"
    | Type.UI (_, Some fields) -> findInFieldList fields
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

module type DATABASE = sig

  type t

  module DataSet : sig
    type t
  end

  val runQuery : t -> TypedQuery.t -> (DataSet.t, string) Result.t

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

  module DataSet = struct
    type t = Js.Json.t
  end

  let runQuery db query =
    let open Result.Syntax in
    let rec aux parent (_typ, query) =
      match query with
      | TypedQuery.Void -> return parent
      | TypedQuery.Here -> return parent
      | TypedQuery.Navigate (query, { name; args = _ }) ->
        let%bind parent = aux parent query in
        let navigate dataset =
          match Js.Json.classify dataset with
          | Js.Json.JSONObject obj ->
            begin match Js.Dict.get obj name with
            | Some dataset -> return dataset
            | None -> error "no such key"
            end
          | _ -> error "expected an object"
        in begin
        match Js.Json.classify parent with
        | Js.Json.JSONObject _ -> navigate parent
        | Js.Json.JSONArray items ->
          let%bind items = Result.Array.map ~f:navigate items in
          return (Js.Json.array items)
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
        in return (Js.Json.object_ dataset)
    in aux db query

end

module Test = struct

  let univ =
    let individual = Type.(
      entity "individual" [
        hasOne "name" string;
        hasOne "site" (entity "site" [
          hasOne "title" string;
        ])
      ]
    ) in
    Universe.(
      empty
      |> hasMany "individual" individual
    )

  (**
    * { individual.pick, individual.code, customField: individual.code }
    *)
  let query = UntypedQuery.Syntax.(
    void
    |> select [
      field ~alias:"individuals" (here |> nav "individual");
      field ~alias:"individual names" (here |> nav "individual" |> nav "name");
      field ~alias:"sites" (here |> nav "individual" |> nav "site");
      field ~alias:"site titles" (here |> nav "individual" |> nav "site" |> nav "title");
    ]
  )

  let query2 = UntypedQuery.Syntax.(
    void |> nav "individual" |> nav "site" |> nav "title"
  )

  let db = JSONDatabase.ofString {|
    {
      "individual": [
          {
          "name": "Andrey Popp",
          "site": {
            "title": "RexDB Site"
          }
        }
      ]
    }
  |}

  let runQuery db query =
    let result =
      let open Result.Syntax in
      let%bind query = Typer.typeQuery univ query in
      JSONDatabase.runQuery db query
    in match result with
    | Ok dataset -> Js.log dataset
    | Error err -> Js.log err

  let () =
    Js.log db;
    runQuery db query;
    runQuery db query2

end
