module StringMap = struct
  include Belt.Map.String
end

module MutStringMap = struct
  include Belt.MutableMap.String
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

  module Syntax = struct
    let one = One
    let opt = Opt
    let many = Many
  end
end

module EqOp = struct
  type t =
    | EQ
    | NEQ

  let show = function
    | EQ -> "="
    | NEQ -> "!="
end

module ComparisonOp = struct
  type t =
    | LT
    | GT
    | LTE
    | GTE

  let show = function
    | LT -> "<"
    | GT -> ">"
    | LTE -> "<="
    | GTE -> ">="
end

module LogicalOp = struct
  type t =
    | AND
    | OR

  let show = function
    | AND -> "&&"
    | OR -> "||"
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
  and arg = t

  let make name value = {name; value}

  let toMap args =
    let build args {name; value} = StringMap.set args name value in
    Belt.List.reduce args StringMap.empty build

  let ofMap args =
    let f args name value = {name;value}::args in
    StringMap.reduce args [] f
end

module Const = struct

  type t =
    | String of string
    | Number of float
    | Bool of bool
    | Null

end

module Mutation = struct
  type 'q t =
    | Update of 'q ops
    | Create of 'q ops

  and 'q ops = 'q op StringMap.t

  and 'q op =
    | OpUpdate of 'q
    | OpUpdateEntity of 'q ops
    | OpCreateEntity of 'q ops

  let rec map f = function
    | Update ops -> Update (mapOps f ops)
    | Create ops -> Create (mapOps f ops)

  and mapOps f ops =
    StringMap.map ops (mapOp f)

  and mapOp f = function
    | OpUpdate q -> OpUpdate (f q)
    | OpUpdateEntity ops ->
      let ops = StringMap.map ops (mapOp f) in
      OpUpdateEntity ops
    | OpCreateEntity ops ->
      let ops = StringMap.map ops (mapOp f) in
      OpCreateEntity ops

  module Syntax = struct

    let collectOps ops =
      let f map (k, v) = StringMap.set map k v in
      Belt.List.reduce ops StringMap.empty f

    let update q = OpUpdate q
    let updateEntity ops = OpUpdateEntity (collectOps ops)
    let createEntity ops = OpCreateEntity (collectOps ops)
  end
end

(**
 * This defines a query syntax parametrized by the payload.
 *
 * Payload can be used to store some semantic info along with a query, for
 * example location of the query sources parsed from source files or type
 * information.
 *)
module Untyped = struct

  type t = context * syntax

  (* This is going to be used for location info *)
  and context = unit

  and syntax =
    | Void
    | Here
    | Select of (t * select)
    | Navigate of t * nav
    | First of t
    | Filter of (t * t)
    | Count of t
    | Screen of (t * screen)
    | Mutation of (t * mutation)
    | Const of Const.t
    | Name of string
    | Define of (t * args)
    | Locate of (t * t)
    | Meta of t
    | Grow of (t * t)
    | GrowArgs of (t * args)
    | ComparisonOp of (ComparisonOp.t * t * t)
    | EqOp of (EqOp.t * t * t)
    | LogicalOp of (LogicalOp.t * t * t)

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

  and mutation = t Mutation.t

  let rec showArgs args =
    let args =
      let f (name, query) =
        let query = show query in
        {j|$name: $query|j}
      in
      args
      |> StringMap.toList
      |> List.map f
      |> String.concat ", "
    in {j|($args)|j}

  and showMutation _ = "MUT"

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
    | Meta parent ->
      let parent = show parent in
      {j|$parent:meta|j}
    | Grow (parent, next) ->
      let parent = show parent in
      let next = show next in
      {j|$parent:grow($next)|j}
    | GrowArgs (parent, args) ->
      let parent = show parent in
      let args = showArgs args in
      {j|$parent$args|j}
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
    | Filter (parent, pred) ->
      let parent = show parent in
      let pred = show pred in
      {j|$parent:filter($pred)|j}
    | Screen (parent, { screenName; screenArgs; }) ->
      let parent = show parent in
      let screenArgs = showArgs screenArgs in
      {j|$parent:$screenName$screenArgs|j}
    | Mutation (parent, mut) ->
      let parent = show parent in
      let mut = showMutation mut in
      {j|$parent:$mut|j}
    | Name name ->
      "$" ^ name
    | Define (parent, args) ->
      let parent = show parent in
      let args = showArgs args in
      {j|$parent:define($args)|j}
    | Locate (parent, id) ->
      let parent = show parent in
      let id = show id in
      {j|$parent[$id]|j}
    | ComparisonOp (op, left, right) ->
      let op = ComparisonOp.show op in
      let left = show left in
      let right = show right in
      {j|$left $op $right|j}
    | EqOp (op, left, right) ->
      let op = EqOp.show op in
      let left = show left in
      let right = show right in
      {j|$left $op $right|j}
    | LogicalOp (op, left, right) ->
      let op = LogicalOp.show op in
      let left = show left in
      let right = show right in
      {j|$left $op $right|j}

  let updateArgs ~(update : args) (args : args) =
    let merge _name a u = match a, u with
    | Some a, None -> Some a
    | None, Some u -> Some u
    | Some _, Some u -> Some u
    | None, None -> None
    in StringMap.merge args update merge

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

    let filter pred parent =
      (), Filter (parent, pred)

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

    let define args parent =
      let args = Arg.toMap args in
      (), Define (parent, args)

    let locate id parent =
      (), Locate (parent, id)

    let meta parent =
      (), Meta parent

    let grow next parent =
      (), Grow (parent, next)

    let growArgs args parent =
      let args = Arg.toMap args in
      (), GrowArgs (parent, args)

    let lessThan left right =
      (), ComparisonOp (ComparisonOp.LT, left, right)

    let lessOrEqThan left right =
      (), ComparisonOp (ComparisonOp.LTE, left, right)

    let greaterThan left right =
      (), ComparisonOp (ComparisonOp.GT, left, right)

    let greaterOrEqThan left right =
      (), ComparisonOp (ComparisonOp.GTE, left, right)

    let eq left right =
      (), EqOp (EqOp.EQ, left, right)

    let notEq left right =
      (), EqOp (EqOp.NEQ, left, right)

    let and_ left right =
      (), LogicalOp (LogicalOp.AND, left, right)

    let or_ left right =
      (), LogicalOp (LogicalOp.OR, left, right)

    let arg = Arg.make

    let collectOps ops =
      let f map (k, v) = StringMap.set map k v in
      Belt.List.reduce ops StringMap.empty f

    let create ops parent =
      (), Mutation (parent, Create (collectOps ops))

    let update ops parent =
      (), Mutation (parent, Update (collectOps ops))

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

  and ctyp = Card.t * t

  and screen = {
    screenName : string;
    screenOut : ctyp;
  }

  and value =
    | String
    | Number
    | Bool
    | Null
    | Abstract

  and entity = {
    entityName : string;
  }

  and field = {
    fieldName : string;
    fieldArgs : args;
    fieldCtyp : ctyp;
  }

  and arg = {
    argCtyp : ctyp;
    argDefault : Untyped.t option;
  }

  and args = arg StringMap.t

  let void = Card.One, Void

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
    | Abstract -> "abstract"

  and showCt (card, typ) =
    let card = Card.show card in
    let typ = show typ in
    {j|$card $typ|j}

  let ctyp =
    let t =
      Card.One,
      Record [
        {
          fieldName = "card";
          fieldCtyp = Card.One, Value String;
          fieldArgs = StringMap.empty;
        };
        {
          fieldName = "type";
          fieldCtyp = Card.One, Value Abstract;
          fieldArgs = StringMap.empty;
        };
      ]
    in
    Card.One, Record [
      {
        fieldName = "type";
        fieldCtyp = t;
        fieldArgs = StringMap.empty;
      };
      {
        fieldName = "registry";
        fieldCtyp = Card.One, Value Abstract;
        fieldArgs = StringMap.empty;
      };
    ]

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

    let entity name = Entity {entityName = name}

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

(**
 * Query with type and cardinality information attached.
 *)
module Typed = struct

  type t = context * syntax

  and context = {
    scope : scope;
    ctyp : Type.ctyp;
  }

  and scope = binding Scope.t

  and binding =
    | TypedBinding of t
    | UntypedBinding of Untyped.t

  and syntax =
    | Void
    | Here
    | Select of (t * select)
    | Navigate of t * nav
    | First of t
    | Filter of (t * t)
    | Count of t
    | Screen of (t * screen)
    | Mutation of (t * mutation)
    | Const of Const.t
    | Name of (Scope.Name.t * t)
    | Define of (t * Untyped.args)
    | Locate of (t * t)
    | Meta of t
    | Grow of (t * t)
    | GrowArgs of (t * Untyped.args)
    | ComparisonOp of (ComparisonOp.t * t * t)
    | EqOp of (EqOp.t * t * t)
    | LogicalOp of (LogicalOp.t * t * t)

  and nav = { navName : string; }

  and screen = {
    screenName : string;
    screenArgs : Untyped.args;
  }

  and select = field list

  and field = {
    alias : string option;
    query: t
  }

  and mutation = t Mutation.t

  let rec stripTypes (q : t) =
    match q with
    | _, Void -> (), Untyped.Void
    | _, Here -> (), Untyped.Here
    | _, Select (parent, fields) ->
      let fields =
        let f {alias; query} = {Untyped. alias; query = stripTypes query} in
        List.map f fields
      in
      (), Untyped.Select (stripTypes parent, fields)
    | _, Navigate (parent, { navName }) ->
      (), Untyped.Navigate (stripTypes parent, {Untyped. navName })
    | _, First parent -> (), Untyped.First (stripTypes parent)
    | _, Filter (parent, pred) ->
      (), Untyped.Filter (stripTypes parent, stripTypes pred)
    | _, Count parent -> (), Untyped.Count (stripTypes parent)
    | _, Screen (parent, { screenName; screenArgs; }) ->
      (), Untyped.Screen (stripTypes parent, {Untyped. screenName; screenArgs; })
    | _, Mutation (parent, mut) ->
      (), Untyped.Mutation (stripTypes parent, Mutation.map stripTypes mut)
    | _, Const v -> (), Untyped.Const v
    | _, Name (name, _) -> (), Untyped.Name (Scope.Name.toString name)
    | _, Define (parent, args) -> (), Untyped.Define (stripTypes parent, args)
    | _, Locate (parent, id) -> (), Locate (stripTypes parent, stripTypes id)
    | _, Meta parent -> (), Untyped.Meta (stripTypes parent)
    | _, Grow (parent, next) -> (), Untyped.Grow (stripTypes parent, stripTypes next)
    | _, GrowArgs (parent, args) -> (), Untyped.GrowArgs (stripTypes parent, args)
    | _, ComparisonOp (op, left, right) ->
      (), Untyped.ComparisonOp (op, stripTypes left, stripTypes right)
    | _, EqOp (op, left, right) ->
      (), Untyped.EqOp (op, stripTypes left, stripTypes right)
    | _, LogicalOp (op, left, right) ->
      (), Untyped.LogicalOp (op, stripTypes left, stripTypes right)

  let show q =
    Untyped.show (stripTypes q)

  module Context = struct
    let updateScope scope ctx =
      {ctx with scope}
  end

  let void = {ctyp = Type.void; scope = Scope.empty}, Void

  let ctyp (q : t) =
    let {ctyp;_}, _ = q in
    ctyp

  let card (q : t) =
    let card, _ = ctyp q in
    card

  let typ (q : t) =
    let _, typ = ctyp q in
    typ
end
