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

  module List = struct

    let rec filterNone = function
      | [] -> []
      | Some x::xs -> x::(filterNone xs)
      | None::xs -> filterNone xs

  end

  let alt a b = match a with
  | Some a -> Some a
  | None -> b

end

module StringMap = Belt.Map.String

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

  module Syntax = struct
    let one = One
    let opt = Opt
    let many = Many
  end

end

(**
 * Helper module for argument like syntax.
 *
 * module Arg = ArgSyntax(struct type t = int end)
 *
 * let args = Arg.[make "a" 1; make "b" 2];
 * let argsAsMap = Arg.toMap args;
 *)
module ArgSyntax (D : sig type t end) = struct

  type t = {name : string; value : D.t}

  let make name value = {name; value}

  let toMap args =
    let build args {name; value} = StringMap.set args name value in
    Belt.List.reduce args StringMap.empty build
end

module ConstExpr = struct

  type t =
    | String of string
    | Number of float
    | Bool of bool
    | Null

end

(**
 * This defines a query syntax parametrized by the payload.
 *
 * Payload can be used to store some semantic info along with a query, for
 * example location of the query sources parsed from source files or type
 * information.
 *)
module Query = struct

  type t = context * syntax

  (* This is going to be used for location info *)
  and context = unit

  and syntax =
    | Void
    | Here
    | Select of (t * select)
    | Navigate of t * nav
    | First of t
    | Count of t
    | Screen of (t * screen)
    | Const of ConstExpr.t
    | Where of (t * args)
    | Name of string
    | Locate of (t * t)

  and args = t StringMap.t

  and nav = { navName : string; }

  and screen = {
    screenName : string;
    screenArgs : args;
  }

  and select = field list

  and field = {
    alias : string option;
    query: t
  }

  let show q =
    let rec showArgs args =
      let args =
        let f acc name query =
          let prefix = match acc with
          | "" -> ""
          | _ -> ", "
          in
          let query = show query in
          {j|$prefix$name: $query|j}
        in
        StringMap.reduce args "" f
      in {j|($args)|j}
    and show (_, syn) =
      match syn with
      | Const (String v) -> {j|"$v"|j}
      | Const (Number v) -> string_of_float v
      | Const (Bool v) -> string_of_bool v
      | Const Null -> "null"
      | Void -> "void"
      | Here -> "here"
      | Count parent ->
        let parent = show parent in
        {j|$parent:count|j}
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
      | Navigate (parent,{ navName; }) ->
        let parent = show parent in
        let this = navName in
        {j|$parent.$this|j}
      | First parent ->
        let parent = show parent in
        {j|$parent:first|j}
      | Screen (parent, { screenName; screenArgs; }) ->
        let parent = show parent in
        let screenArgs = showArgs screenArgs in
        {j|$parent:$screenName$screenArgs|j}
      | Name name ->
        "$" ^ name
      | Locate (parent, id) ->
        let parent = show parent in
        let id = show id in
        {j|$parent[$id]|j}
      | Where (parent, bindings) ->
        let parent = show parent in
        let bindings = showArgs bindings in
        {j|$parent:where($bindings)|j}
    in show q

  let unsafeLookupArg ~name args =
    StringMap.getExn args name

  let updateArgs ~(update : args) (args : args) =
    let merge _name a u = match a, u with
    | Some a, None -> Some a
    | None, Some u -> Some u
    | Some _, Some u -> Some u
    | None, None -> None
    in StringMap.merge args update merge

  let rec chain ((), rootSyn as root) ((), syn) =
    let syn = match syn with
    | Here -> rootSyn
    | Void -> Void
    | Select (parent, fields) -> Select (chain root parent, fields)
    | Navigate (parent, nav) -> Navigate (chain root parent, nav)
    | First parent -> First (chain root parent)
    | Count parent -> Count (chain root parent)
    | Screen (parent, screen) -> Screen (chain root parent, screen)
    | Const v -> Const v
    | Where (parent, bindings) -> Where (chain root parent, bindings)
    | Name name -> Name name
    | Locate (parent, id) -> Locate (chain root parent, chain root id)
    in (), syn

  module Syntax = struct

    module Arg = ArgSyntax(struct type nonrec t = t end)

    let void =
      (), Void

    let here =
      (), Here

    let nav name parent =
      (), Navigate (parent, { navName = name; })

    let select fields parent =
      (), Select (parent, fields)

    let field ?alias query =
      { query; alias; }

    let screen ?(args=[]) name parent =
      let screenArgs = Arg.toMap args in
      (), Screen (parent, { screenName = name; screenArgs; })

    let count parent =
      (), Count parent

    let first parent =
      (), First parent

    let string v =
      (), Const (String v)

    let number v =
      (), Const (Number v)

    let bool v =
      (), Const (Bool v)

    let null =
      (), Const Null

    let name name =
      (), Name name

    let where bindings parent =
      let bindings = Arg.toMap bindings in
      (), Where (parent, bindings)

    let locate id parent =
      (), Locate (parent, id)

    let arg = Arg.make
    let define = Arg.make

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
    screenIn : ct;
    screenOut : ct;
    screenOutQuery : Query.t;
  }

  and value =
    | String
    | Number
    | Bool
    | Null

  and entity = {
    entityName : string;
    entityFields : field list option;
  }

  and field = {
    fieldName : string;
    fieldArgs : args;
    fieldCtyp : ct;
  }

  and arg = {
    argCtyp : ct;
    argDefault : Query.t option;
  }

  and args = arg StringMap.t

  let rec show = function
    | Void -> "void"
    | Value vt -> showValue vt
    | Screen { screenName; _ } -> {j|Screen("$screenName")|j}
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

  and showValue = function
    | String -> "string"
    | Number -> "number"
    | Bool -> "bool"
    | Null -> "null"

  and showCt (card, typ) =
    let card = Card.show card in
    let typ = show typ in
    {j|$card $typ|j}

  (**
   * Combinators to define a type system.
   *)
  module Syntax = struct

    module Arg = struct
      module ArgSyntax = ArgSyntax(struct type t = arg end)

      type arg = ArgSyntax.t

      let arg ?default name argCtyp =
        let arg = {argCtyp; argDefault = default} in
        ArgSyntax.make name arg
    end

    let entity name fields = Entity {entityName = name; entityFields = Some fields}

    let has ?(card=Card.One) ?(args=[]) name typ =
      let fieldArgs = Arg.ArgSyntax.toMap args in
      {
        fieldName = name;
        fieldArgs;
        fieldCtyp = card, typ;
      }

    let hasOne = has ~card:Card.One
    let hasOpt = has ~card:Card.Opt
    let hasMany = has ~card:Card.Many

    module Card = struct
      let one typ = Card.One, typ
      let opt typ = Card.Opt, typ
      let many typ = Card.Many, typ
    end

    include Card

    module Value = struct
      let string = Value String
      let number = Value Number
      let bool = Value Bool
    end

    include Value

    include Arg

  end

end

module Context = struct

  type t = scope * Type.ct

  and scope = Query.t StringMap.t

  let void = StringMap.empty, (Card.One, Type.Void)

  let inspect (scope, ctyp) =
    let scope =
      let f dict k v =
        Js.Dict.set dict k (Query.show v);
        dict
      in
      StringMap.reduce scope (Js.Dict.empty ()) f
    in
    [%bs.obj {scope; ctyp = Type.showCt ctyp}]

end

(**
 * Query with type and cardinality information attached.
 *)
module TypedQuery = struct

  type t = context * syntax

  and context = Context.t

  and syntax =
    | Void
    | Here
    | Select of (t * select)
    | Navigate of t * nav
    | First of t
    | Count of t
    | Screen of (t * screen)
    | Const of ConstExpr.t
    | Where of (t * args)
    | Name of (string * Context.t)
    | Locate of (t * t)

  and args = Query.t StringMap.t

  and nav = { navName : string; }

  and screen = {
    screenName : string;
    screenArgs : args;
  }

  and select = field list

  and field = {
    alias : string option;
    query: t
  }

  let void = Context.void, Void

  let rec stripTypes (q : t) =
    match q with
    | _, Void -> (), Query.Void
    | _, Here -> (), Query.Here
    | _, Select (parent, fields) ->
      let fields =
        let f {alias; query} = {Query. alias; query = stripTypes query} in
        List.map f fields
      in
      (), Query.Select (stripTypes parent, fields)
    | _, Navigate (parent, { navName }) ->
      (), Query.Navigate (stripTypes parent, {Query. navName })
    | _, First parent -> (), Query.First (stripTypes parent)
    | _, Count parent -> (), Query.Count (stripTypes parent)
    | _, Screen (parent, { screenName; screenArgs; }) ->
      (), Query.Screen (stripTypes parent, {Query. screenName; screenArgs; })
    | _, Const v -> (), Query.Const v
    | _, Where (parent, bindings) -> (), Query.Where (stripTypes parent, bindings)
    | _, Name (name, _) -> (), Query.Name name
    | _, Locate (parent, id) -> (), Locate (stripTypes parent, stripTypes id)

  let show q =
    Query.show (stripTypes q)

end

(**
 * Query result which extends JSON type with a special UI type.
 *
 * It is implemented as a zero (almost) cost on top of native JS data
 * structures.
 *)
module Value = struct

  type t
  type value = t

  (**
  * This is an opaque structure which defines UI.
  *)
  module UI : sig

    type t

    val make :
      name : string
      -> args : TypedQuery.args
      -> typ : Type.t
      -> value : value
      -> query : TypedQuery.t
      -> outQuery : Query.t
      -> t

    val test : 'a -> bool

    val name : t -> string
    val args : t -> TypedQuery.args
    val setArgs : args : TypedQuery.args -> t -> t
    val query : t -> TypedQuery.t
    val outQuery : t -> Query.t
    val value : t -> value
    val typ : t -> Type.t

  end = struct
    type t = <
      name : string;
      args : TypedQuery.args;
      query : TypedQuery.t;
      outQuery : Query.t;
      value : value;
      typ : Type.t;
    > Js.t

    external make :
      name : string
      -> args : TypedQuery.args
      -> typ : Type.t
      -> value : value
      -> query : TypedQuery.t
      -> outQuery : Query.t
      -> t
      = "UIRepr" [@@bs.new] [@@bs.module "./UIRepr"]

    let test_ : 'a -> bool = [%bs.raw {|
      function test(v) { return v instanceof UIRepr.UIRepr; }
    |}]

    let test x = test_ (Obj.magic x)

    let name ui = ui##name
    let args ui = ui##args
    let query ui = ui##query
    let outQuery ui = ui##outQuery
    let value ui = ui##value
    let typ ui = ui##typ

    let setArgs ~args ui =
      let args = Query.updateArgs ~update:args ui##args in
      make ~name:ui##name ~args ~typ:ui##typ ~value:ui##value ~query:ui##query ~outQuery:ui##outQuery
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

  let get ~name value =
    match classify value with
    | Object value -> Js.Dict.get value name
    | _ -> None

end

module Screen = struct

  type t = {
    args : Type.arg StringMap.t;
    inputCard : Card.t;
    grow : Query.t;
  }

  module Syntax = struct

    include Type.Syntax.Value
    include Type.Syntax.Card
    include Type.Syntax.Arg

    let screen ?(args=[]) ~inputCard grow =
      let args = Type.Syntax.Arg.ArgSyntax.toMap args in {
        args;
        inputCard;
        grow;
      }

    let has ?(args=[]) ~resolve fieldName fieldCtyp =
      let args = Type.Syntax.Arg.ArgSyntax.toMap args in
      let field = {
        Type.
        fieldName;
        fieldArgs = args;
        fieldCtyp;
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

  val hasOne : ?args : Type.Syntax.arg list -> string -> Type.t -> t -> t
  val hasOpt : ?args : Type.Syntax.arg list -> string -> Type.t -> t -> t
  val hasMany : ?args : Type.Syntax.arg list -> string -> Type.t -> t -> t

  val hasScreen : string -> Screen.t -> t -> t

  val fields : t -> Type.field list

  val lookupScreen : string -> t -> Screen.t option

end = struct

  type t = {
    fields : Type.field list;
    screens : Screen.t StringMap.t;
  }

  let empty = { fields = []; screens = StringMap.empty }

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
    { univ with screens = StringMap.set univ.screens name screen; }

  let fields univ = univ.fields

  let lookupScreen name univ =
    StringMap.get univ.screens name
end

(**
 * This module implements a type checking / type inferrence for query structure
 * by turning untype queries into typed ones.
 *)
module QueryTyper : sig

  val typeQuery :
    ?ctx : TypedQuery.context
    -> univ:Universe.t
    -> Query.t
    -> (TypedQuery.t, string) Result.t

  val checkArgs :
    argTyps : Type.args
    -> Query.args
    -> (TypedQuery.args, string) Result.t

  (** Same as checkArgs but doesn't set default values for missing args *)
  val checkArgsPartial :
    argTyps : Type.args
    -> Query.args
    -> (TypedQuery.args, string) Result.t

  val nav :
    univ:Universe.t
    -> string
    -> TypedQuery.t
    -> (TypedQuery.t, string) Result.t

end = struct

  let rec extractField ~univ fieldName (typ : Type.t) =
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
    | Type.Screen { screenOut = _, typ; _ } ->
      extractField ~univ fieldName typ
    | Type.Entity {entityName; entityFields = None} ->
      error {j|cannot extract field "$fieldName" from entity "$entityName"|j}
    | Type.Entity {entityName = _; entityFields = Some fields} -> findInFieldList fields
    | Type.Record fields -> findInFieldList fields
    | Type.Value _ ->
      error {j|cannot extract field "$fieldName" from value|j}

  let nav ~univ name parent =
    let open Result.Syntax in
    let navigation = { TypedQuery. navName = name; } in
    let (scope, (parentCard, parentTyp)), _parentSyn = parent in
    Js.log3 "CH" name (Type.show parentTyp);
    let%bind field = extractField ~univ name parentTyp in
    let fieldCard, fieldTyp = field.fieldCtyp in
    Js.log2 "OK" (Type.show fieldTyp);
    let fieldCard = Card.merge parentCard fieldCard in
    return ((scope, (fieldCard, fieldTyp)), TypedQuery.Navigate (parent, navigation))

  let rec checkArgsImpl
    ~updateWithDefaultValues
    ~argTyps
    args
    =
    let open Result.Syntax in
    (** Type check all passed args first *)
    let%bind args =
      let f args name query =
        let%bind args = args in
        match StringMap.get argTyps name with
        | None -> error {j|unknown arg "$name"|j}
        | Some _ ->
          let args = StringMap.set args name query in
          return args
      in StringMap.reduce args (return StringMap.empty) f
    in
    (** Now check if we didn't miss any required arg, add default values *)
    let%bind args =
      if updateWithDefaultValues
      then
        let f args name { Type. argCtyp = _; argDefault } =
          let%bind args = args in
          match StringMap.get args name with
          | None ->
            begin match argDefault with
            | None -> error {j|missing required arg "$name"|j}
            | Some query ->
              let args = StringMap.set args name query in
              return args
            end
          | Some _ -> return args
        in StringMap.reduce argTyps (return args) f
      else return args
    in
    return args

  and typeQuery ?(ctx=Context.void) ~univ query =
    Js.log2 "typeQuery query" (Query.show query);
    Js.log2 "typeQuery ctx" (Context.inspect ctx);
    let rec aux ~ctx ((), query) =
      let scope, ctyp = ctx in
      let open Result.Syntax in
      match query with
      | Query.Void ->
        return ((scope, (Card.One, Type.Void)), TypedQuery.Void)
      | Query.Here ->
        return (ctx, TypedQuery.Here)
      | Query.Locate (parent, id) ->
        let%bind ((bindings, (card, typ)), _) as parent = aux ~ctx parent in
        begin match card with
        | Card.Many ->
          let%bind id = aux ~ctx id in
          return ((bindings, (Card.Opt, typ)), TypedQuery.Locate (parent, id))
        | _ ->
          error {j|locate can only be applied to queries with cardinality many|j}
        end
      | Query.Name name ->
        begin match StringMap.get scope name with
        | Some query ->
          let%bind nextCtx, _syn = aux ~ctx query in
          return (nextCtx, TypedQuery.Name (name, ctx))
        | None ->
          Js.log (Context.inspect ctx);
          error {j|referencing unknown binding "$name"|j}
        end
      | Query.Where (parent, bindings) ->
        let nextScope =
          let f scope name query =
            StringMap.set scope name query
          in
          StringMap.reduce bindings scope f
        in
        let%bind ((_, parentCtyp), _) as parent = aux ~ctx:(nextScope, ctyp) parent in
        return (
          (scope, parentCtyp),
          TypedQuery.Where (parent, bindings)
        )
      | Query.Const (ConstExpr.String v) ->
        return (
          (scope, (Card.One, Type.Value Type.String)),
          TypedQuery.Const (ConstExpr.String v)
        )
      | Query.Const (ConstExpr.Number v) ->
        return (
          (scope, (Card.One, Type.Value Type.Number)),
          TypedQuery.Const (ConstExpr.Number v)
        )
      | Query.Const (ConstExpr.Bool v) ->
        return (
          (scope, (Card.One, Type.Value Type.Bool)),
          TypedQuery.Const (ConstExpr.Bool v)
        )
      | Query.Const (ConstExpr.Null) ->
        return (
          (scope, (Card.One, Type.Value Type.Null)),
          TypedQuery.Const (ConstExpr.Null)
        )
      | Query.Count parent ->
        let%bind parent = aux ~ctx parent in
        return (
          (scope, (Card.One, Type.Syntax.number)),
          TypedQuery.Count parent
        )
      | Query.First parent ->
        let%bind ((_, (_, parentType)), _) as parent = aux ~ctx parent in
        return (
          (scope, (Card.Opt, parentType)),
          TypedQuery.First parent
        )
      | Query.Screen (parent, { screenName; screenArgs; }) ->
        let parentUntyped = parent in
        let%bind (parentCtx, _) as parent = aux ~ctx parent in

        let%bind screen = Result.ofOption
          ~err:{j|no such screen "$screenName"|j}
          (Universe.lookupScreen screenName univ)
        in

        let%bind screenArgs = checkArgsImpl
          ~updateWithDefaultValues:true
          ~argTyps:screen.args screenArgs
        in

        let%bind ctx =
          let parentScope, ((card, _typ) as parentCtyp) = parentCtx in
          match (screen.inputCard, card) with
          | Card.One, Card.Many
          | Card.Opt, Card.Many
          | Card.Many, Card.One
          | Card.Many, Card.Opt ->
            error {j|screen "$screenName" cannot be constructed due to cardinality mismatch|j}
          | Card.One, Card.Opt
          | Card.One, Card.One
          | Card.Opt, Card.Opt
          | Card.Opt, Card.One
          | Card.Many, Card.Many ->
            let bindings =
              let bindings = StringMap.empty in
              let bindings =
                match StringMap.get parentScope "parent" with
                | Some _ -> bindings
                | None -> StringMap.set bindings "parent" parentUntyped
              in
              let bindings = StringMap.reduce screenArgs bindings StringMap.set in
              bindings
            in
            let outQuery = (), Query.Where (screen.grow, bindings) in
            Js.log2 "SSS" (Query.show outQuery);
            let%bind (_, screenOut), _ = aux ~ctx:parentCtx outQuery in
            Js.log "SSS";
            return (
              parentScope,
              (
                Card.One,
                Type.Screen { screenName; screenIn = parentCtyp; screenOut; screenOutQuery = outQuery; }
              )
            )
        in

        return (
          ctx,
          TypedQuery.Screen (parent, { screenName; screenArgs; })
        )
      | Query.Navigate (parent, navigation) ->
        let { Query. navName; } = navigation in
        let%bind parent = aux ~ctx parent in
        nav ~univ navName parent
      | Query.Select (parent, selection) ->
        let%bind parent = aux ~ctx parent in
        let parentCtx, _parentSyn = parent in
        let parentScope, (parentCard, _parentTyp) = parentCtx in
        let checkField fields { Query. alias; query } =
          match fields with
          | Result.Ok (fields, selection, index) ->
            let%bind query = aux ~ctx:parentCtx query in
            let (_fieldScope, (fieldCard, fieldTyp)), _fieldSyn = query in
            let fieldName = Option.getWithDefault (string_of_int index) alias in
            let fieldCard = Card.merge parentCard fieldCard in
            let fieldCtyp = fieldCard, fieldTyp in
            let field = { Type. fieldCtyp; fieldName; fieldArgs = StringMap.empty } in
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
        return ((parentScope, (parentCard, typ)), TypedQuery.Select (parent, selection))
    in aux ~ctx query

  let checkArgs = checkArgsImpl ~updateWithDefaultValues:true
  let checkArgsPartial = checkArgsImpl ~updateWithDefaultValues:false
end

(**
 * Abstract interface to the database.
 *)
module type DATABASE = sig

  type t

  val univ : t -> Universe.t

  val execute : ?value : Value.t -> db : t -> TypedQuery.t -> (Value.t, string) Result.t

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

  let ofJson ~univ root =
    { root = Value.ofJson root; univ }

  let univ db = db.univ

  let execute ?value ~db query =
    Js.log2 "EXECUTE" (TypedQuery.show query);
    let open Result.Syntax in
    let rec aux ~(value : Value.t) ((bindings, (_card, typ)), syn) =
      match typ, syn with
      | Type.Void, TypedQuery.Void ->
        return db.root
      | _, TypedQuery.Void ->
        error "invalid type for void"

      | _, TypedQuery.Here ->
        return value

      | _, TypedQuery.Name (name, ctx) ->
        begin match StringMap.get bindings name with
        | None -> error {j|no such query defined: "$name"|j}
        | Some query ->
          let%bind query = QueryTyper.typeQuery ~univ:(univ db) ~ctx query in
          aux ~value query
        end

      | _, TypedQuery.Where (parent, _bindings) ->
        aux ~value parent

      | Type.Value Type.String, TypedQuery.Const (ConstExpr.String v) ->
        return (Value.string v)
      | Type.Value Type.Number, TypedQuery.Const (ConstExpr.Number v) ->
        return (Value.number v)
      | Type.Value Type.Bool, TypedQuery.Const (ConstExpr.Bool v) ->
        return (Value.bool v)
      | Type.Value Type.Null, TypedQuery.Const (ConstExpr.Null) ->
        return (Value.null)
      | _, TypedQuery.Const _ ->
        error "invalid type for const"

      | Type.Value Type.Number, TypedQuery.Count query ->
        let%bind value = aux ~value query in
        begin match Value.classify value with
        | Value.Null -> return (Value.number 0.)
        | Value.Array items -> return (Value.number (float_of_int (Array.length items)))
        | _ -> return (Value.number 1.)
        end
      | _, TypedQuery.Count _ ->
        error "invalid type for count"

      | _, TypedQuery.First query ->
        let%bind value = aux ~value query in
        begin match Value.classify value with
        | Value.Array items ->
          if Array.length items > 0
          then return (Array.get items 0)
          else return Value.null
        | _ -> return value
        end

        | Type.Screen { screenOutQuery = outQuery; }, TypedQuery.Screen (query, { screenName; screenArgs; }) ->

        let make value =
          match Value.classify value with
          | Value.Null -> return Value.null
          | _ ->
            return (Value.ui (Value.UI.make ~name:screenName ~args:screenArgs ~typ ~value ~query ~outQuery))
        in

        let%bind screen = Result.ofOption
          ~err:{j|no such screen "$screenName"|j}
          (Universe.lookupScreen screenName db.univ)
        in
        begin match screen.Screen.inputCard with
        | Card.One ->
          (* If package expects cardinality One we do a prefetch, ideally we
           * should prefetch just exists(query) instead of query itself so we
           * can minimize the work for db to do.
           *)
          let%bind prefetch = aux ~value query in
          begin match Value.classify prefetch with
          | Value.Null -> return Value.null
          | _ -> make value
          end
        | Card.Opt
        | Card.Many -> make value
        end
      | _, TypedQuery.Screen _ ->
        error "invalid type for screen"

      | _, TypedQuery.Navigate (query, { navName; }) ->
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
          | Value.Null -> return Value.null
          | _ -> error "cannot traverse this"
        in

        begin match Value.classify value with
        | Value.Object _ -> navigate navName value
        | Value.Array items  ->
          let%bind items = Result.Array.map ~f:(navigate navName) items in
          let items =
            let f res item =
              match Value.classify item with
              | Value.Array item -> Belt.Array.concat res item
              | _ -> ignore (Js.Array.push item res); res
            in
            Belt.Array.reduce items (Belt.Array.makeUninitializedUnsafe 0) f in
          return (Value.array items)
        | Value.Null ->
          return Value.null
        | Value.UI ui ->
          let outQuery = Value.UI.outQuery ui in
          let queryValue = Value.UI.value ui in
          let%bind outQuery = QueryTyper.typeQuery ~univ:(univ db) outQuery in
          let%bind value = aux ~value:queryValue outQuery in
          navigate navName value
        | _ -> error {|Cannot navigate away from this value|}
        end

      | Type.Record _, TypedQuery.Select (query, selection) ->
        let selectFrom value =
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
          in
          return (Value.obj dataset)
        in
        let%bind value = aux ~value query in
        begin match Value.classify value with
        | Value.Object _ -> selectFrom value
        | Value.Array items ->
          let%bind items = Result.Array.map ~f:selectFrom items in
          return (Value.array items)
        | Value.Null ->
          return Value.null
        | _ ->
          error "cannot select from here"
        end
      | _, TypedQuery.Select _ ->
        error "invalid type for select"

      | _, TypedQuery.Locate (parent, id) ->
        let%bind parent = aux ~value parent in
        begin match Value.classify parent with
        | Value.Array items ->
          let%bind id = aux ~value id in
          let f v =
            match Value.get ~name:"id" v with
            | Some v -> v = id
            | None -> false
          in
          return (Value.ofOption (Js.Array.find f items))
        | _ -> error "expected array"
        end

    in
    let value = match value with
    | Some value -> value
    | None -> db.root
    in
    let%bind res = aux ~value query in
    Js.log2 "EXECUTE RESULT" res;
    return res

end

(**
 * Monadic structure on top queries which represent transition between screens.
 *)
module Workflow (Q : sig
  type t
  val show : t -> string
end) = struct

  type q = Q.t
  type t =
    (** Render concrete query to a screen *)
    | Render of q
    (** Define how to transition from one screen to another screen *)
    | Next of (t * t list)

  let rec show v =
    match v with
    | Render q ->
        let q = Q.show q in {j|render($q)|j}
    | Next (w, next) ->
      let w = show w
      and next = next |> List.map show |> String.concat ", "
      in {j|$w { $next }|j}

end

module UntypedWorkflow = struct
  include Workflow(Query)

  module Syntax = struct
    let render q = Render q
    let andThen path w = Next (w, path)
  end
end

module TypedWorkflow = struct
  include Workflow(struct

    type t = TypedQuery.t

    let show w =
      TypedQuery.show w

  end)
end

module WorkflowTyper : sig

  val typeWorkflow :
    univ : Universe.t
    -> UntypedWorkflow.t
    -> (TypedWorkflow.t, string) Result.t

end = struct

  let typeWorkflow ~univ w =
    let open Result.Syntax in
    let rec aux ~parent w =
      match w with
      | UntypedWorkflow.Render q ->
        Js.log2 "TYPING" (Query.show q);
        let q = Query.chain parent q in
        let%bind q = QueryTyper.typeQuery ~univ q in
        Js.log2 "TYPING OK" (TypedQuery.show q);
        begin match q with
        | (_, (_, Type.Screen { screenOutQuery; _ })), _ ->
          return (TypedWorkflow.Render q, screenOutQuery)
        | (_, (_, typ)), _ ->
          let typ = Type.show typ in
          let msg = {j|workflow can only be defined with screen but got $typ|j} in
          error msg
        end
      | UntypedWorkflow.Next (first, next) ->
        let%bind first, parent = aux ~parent first in
        let%bind next, _ =
          let f (next, parent) w =
            let%bind w, _ = aux ~parent w in
            return (w::next, parent)
          in
          Result.List.foldLeft ~f ~init:([], parent) next
        in
        return (TypedWorkflow.Next (first, List.rev next), parent)
    in
    let%bind tw, _ = aux ~parent:Query.Syntax.void w in
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
  val boot : db : Db.t -> TypedWorkflow.t -> ((t * Value.UI.t option), string) Result.t

  (**
   * Render workflow state and return a new state and a UI screen to render.
   *)
  val render : t -> ((t * Value.UI.t option), string) Result.t

  val dataQuery : t -> (TypedQuery.t, string) Result.t

  val titleQuery : t -> (TypedQuery.t, string) Result.t

  val uiQuery : t -> (TypedQuery.t, string) Result.t

  val setArgs : args : Query.args -> t -> (t, string) Result.t

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
    args : TypedQuery.args;
  }

  and position =
    | Root
    | First of t
    | Next of t

  let uiQuery (frame, _) =
    let open Result.Syntax in
    match frame.workflow with
    | TypedWorkflow.Render q ->
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

  let make
    ?prev
    ?ui
    ?(args=StringMap.empty)
    ?(query=(Context.void, TypedQuery.Void))
    ~position
    ~db
    workflow
    =
    let frame = {
      query;
      db;
      workflow;
      position;
      prev;
      args;
    } in frame, ui

  let findRender startState =
    let open Result.Syntax in

    let rec aux (frame, _ as state) =
      let {workflow; db; query; _} = frame in
      match workflow with
      | TypedWorkflow.Next (first, _next) ->
        let state = make ?prev:frame.prev ~position:(First state) ~query ~db first in
        aux state
      | TypedWorkflow.Render _ ->
        let%bind q = uiQuery state in
        let%bind ui = Db.execute ~db:frame.db q in
        begin match Value.classify ui with
        | Value.Null -> return None
        | Value.UI _ -> return (Some state)
        | _ -> error "not an ui"
        end
    in

    aux startState

  let render state =
    let open Result.Syntax in

    let render (frame, _ as state) =
      let%bind q = uiQuery state in
      Js.log2 "WorkflowInterpreter.render" (TypedQuery.show q);
      let%bind res = Db.execute ~db:frame.db q in
      match Value.classify res with
      | Value.UI ui ->
        let ui = Value.UI.setArgs ~args:frame.args ui in
        return ((frame, Some ui), Some ui)
      | Value.Null ->
        return ((frame, None), None)
      | _ -> error "expected UI, got data"
    in

    match%bind findRender state with
    | Some state -> render state
    | None -> return (state, None)

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
      | TypedWorkflow.Next (_first, next), _ ->
        let f w =
          let state = make ~prev:currentState ~query ~position:(Next state) ~db w in
          findRender state
        in
        let%bind next = Result.List.map ~f next in
        return (Option.List.filterNone next)
    in

    (* First we need to produce the output query for the current state *)
    let%bind q = uiQuery currentState in
    let%bind q =
      match startFrame.workflow with
      | Render q ->
        let%bind value = Db.execute ~db:startFrame.db q in
        begin match Value.classify value with
        | Value.UI ui ->
          let outQuery = Value.UI.outQuery ui in
          let%bind outQuery = QueryTyper.typeQuery ~univ:(Db.univ startFrame.db) outQuery in
          return outQuery
        | _ -> error "expected UI"
        end
      | _ -> return q
    in

    let%bind r = aux q currentState in
    return r

  let step state =
    let open Result.Syntax in
    let%bind next = next state in
    Js.log2 "NEXT" (List.map show next |> Array.of_list);
    match next with
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
    let open Result.Syntax in
    let%bind frame = match frame.workflow with
    | TypedWorkflow.Render (ctyp, TypedQuery.Screen (p, c)) ->
      let univ = Db.univ frame.db in
      let%bind screen = Result.ofOption
        ~err:(let name = c.screenName in {j|no such screen "$name"|j})
        (Universe.lookupScreen c.screenName univ)
      in
      let%bind args = QueryTyper.checkArgsPartial ~argTyps:screen.args args in
      let c = {
        c with TypedQuery.
        screenArgs = Query.updateArgs ~update:args c.screenArgs
      } in
      let workflow = TypedWorkflow.Render (ctyp, TypedQuery.Screen (p, c)) in
      return { frame with args; workflow; }
    | _ -> error {j|Arguments can only be updated at the workflow node with a rendered screen|j}
    in
    return (frame, ui)

end

module ParseResult = struct
  type t =
    | Workflow of UntypedWorkflow.t
    | Query of Query.t
end
