(**
 * Query
 *)

(**
 * Query Cardinality
 *)
module Card : sig

  type t =
    | One
    | Opt
    | Many

  val merge : t -> t -> t

  val show : t -> string

  module Syntax : sig
    val one : t
    val opt : t
    val many : t
  end
end

(**
 * Constant values tagged with types.
 *)
module Const : sig

  type t =
    | String of string
    | Number of float
    | Bool of bool
    | Null
end

(**
 * Untyped query syntax.
 *)
module Untyped : sig

  type t = context * syntax

  and context = unit

  and syntax =
      Void
    | Here
    | Select of (t * select)
    | Navigate of t * nav
    | First of t
    | Count of t
    | Screen of (t * screen)
    | Mutation of (t * mutation)
    | Const of Const.t
    | Name of string
    | Locate of (t * t)
    | Meta of t
    | Grow of (t * t)
    | GrowArgs of (t * args)
    | LessThan of (t * t)

  and args = t Common.StringMap.t

  and nav = { navName : string; }

  and screen = { screenName : string; screenArgs : args; }

  and select = field list

  and field = { alias : string option; query : t; }

  and mutation =
    | Update of ops
    | Create of ops

  and ops = op Common.StringMap.t

  and op =
    | OpUpdate of t
    | OpUpdateEntity of ops
    | OpCreateEntity of ops

  val show : t -> string

  val showArgs : args -> string

  val updateArgs : update:args -> args -> t Common.StringMap.t

  module Syntax : sig

    module Arg : sig
      type arg

      val make : string -> t -> arg
      val ofMap : t Common.StringMap.t -> arg list
    end

    val void : t
    val here : t
    val nav : string -> t -> t
    val select : select -> t -> t
    val field : ?alias:string -> t -> field
    val screen : ?args:Arg.arg list -> string -> t -> t
    val count : t -> t
    val first : t -> t
    val string : string -> t
    val number : float -> t
    val bool : bool -> t
    val null : t
    val name : string -> t
    val locate : t -> t -> t
    val meta : t -> t
    val grow : t -> t -> t
    val growArgs : Arg.arg list -> t -> t
    val lessThan : t -> t -> t
    val arg : string -> t -> Arg.arg
    val define : string -> t -> Arg.arg
    val update : (string * op) list -> t -> t
    val create : (string * op) list -> t -> t
    val opUpdate : t -> op
    val opUpdateEntity : (string * op) list -> op
    val opCreateEntity : (string * op) list -> op
  end
end

module Type : sig
  type t =
    | Void
    | Screen of screen
    | Entity of entity
    | Record of field list
    | Value of value

  and ctyp = Card.t * t

  and screen = { screenName : string; screenOut : ctyp; }

  and value =
    | String
    | Number
    | Bool
    | Null
    | Abstract

  and entity = { entityName : string; entityFields : t -> field list; }

  and field = { fieldName : string; fieldArgs : args; fieldCtyp : ctyp; }

  and arg = { argCtyp : ctyp; argDefault : Untyped.t option; }

  and args = arg Common.StringMap.t

  val void : ctyp

  val show : t -> string

  val showValue : value -> string

  val showCt : ctyp -> string

  val ctyp : Card.t * t

  module Syntax : sig
    module Arg : sig
      module ArgSyntax : sig
        type t = { name : string; value : arg; }
        val make : string -> arg -> t
        val toMap : t Belt.List.t -> arg Common.StringMap.t
        val ofMap : arg Common.StringMap.t -> t list
      end
      type arg = ArgSyntax.t
      val arg : ?default:Untyped.t -> string -> ctyp -> ArgSyntax.t
    end

    val entity : string -> (t -> field list) -> t
    val has :
      ?card:Card.t ->
      ?args:Arg.ArgSyntax.t Belt.List.t -> string -> t -> field
    val hasOne : ?args:Arg.ArgSyntax.t Belt.List.t -> string -> t -> field
    val hasOpt : ?args:Arg.ArgSyntax.t Belt.List.t -> string -> t -> field
    val hasMany : ?args:Arg.ArgSyntax.t Belt.List.t -> string -> t -> field
    val one : 'a -> Card.t * 'a
    val opt : 'a -> Card.t * 'a
    val many : 'a -> Card.t * 'a
    module Value : sig val string : t val number : t val bool : t end
    val string : t
    val number : t
    val bool : t
    module ArgSyntax = Arg.ArgSyntax
    type arg = ArgSyntax.t
    val arg : ?default:Untyped.t -> string -> ctyp -> ArgSyntax.t

    module Card : sig

      val one : t -> ctyp

      val opt : t -> ctyp

      val many : t -> ctyp
    end
  end
end

module Typed : sig
  type t = Type.ctyp * syntax

  and scope = binding Common.StringMap.t

  and binding =
    | UntypedBinding of Untyped.t
    | TypedBinding of t
    | HardBinding of t

  and syntax =
    | Void
    | Here of t
    | Select of (t * select)
    | Navigate of t * nav
    | First of t
    | Count of t
    | Screen of (t * screen)
    | Mutation of (t * mutation)
    | Const of Const.t
    | Name of (string * t)
    | Locate of (t * t)
    | Meta of t
    | Grow of (t * t)
    | GrowArgs of (t * Untyped.args)
    | LessThan of (t * t)

  and nav = { navName : string; }

  and screen = { screenName : string; screenArgs : Untyped.args; }

  and select = field list

  and field = { alias : string option; query : t; }

  and mutation =
    | Update of Untyped.ops
    | Create of Untyped.ops

  val stripTypes : t -> Untyped.t

  val show : t -> string

  module Scope : sig
    include module type of Common.StringMap
  end

  val void : t

  val ctyp : t -> Type.ctyp

  val card : t -> Card.t

  val typ : t -> Type.t
end
