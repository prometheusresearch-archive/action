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

module Operator : sig

  type t =
    | Equal
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual

  val show : t -> string

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
    | Const of Const.t
    | Where of (t * args)
    | Name of string
    | Locate of (t * t)
    | Meta of t
    | Grow of (t * t)
    | Compare of binary

  and binary = { op : Operator.t; left : t; right : t}

  and args = t Common.StringMap.t

  and nav = { navName : string; }

  and screen = { screenName : string; screenArgs : args; }

  and select = field list

  and field = { alias : string option; query : t; }

  val show : t -> string

  val showArgs : args -> string

  val updateArgs : update:args -> args -> t Common.StringMap.t

  module Syntax : sig

    module Arg : sig
      type arg

      val make : string -> t -> arg
      val ofMap : t Common.StringMap.t -> arg list
    end

    val void : unit * syntax
    val here : unit * syntax
    val nav : string -> t -> unit * syntax
    val select : select -> t -> unit * syntax
    val field : ?alias:string -> t -> field
    val screen : ?args:Arg.arg Belt.List.t -> string -> t -> unit * syntax
    val count : t -> unit * syntax
    val first : t -> unit * syntax
    val string : string -> unit * syntax
    val number : float -> unit * syntax
    val bool : bool -> unit * syntax
    val null : unit * syntax
    val name : string -> unit * syntax
    val where : Arg.arg Belt.List.t -> t -> unit * syntax
    val locate : t -> t -> unit * syntax
    val meta : t -> unit * syntax
    val grow : t -> t -> unit * syntax
    val lessThan : t -> t -> unit * syntax
    val arg : string -> t -> Arg.arg
    val define : string -> t -> Arg.arg
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
  type t = context * syntax

  and context = scope * Type.ctyp

  and scope = binding Common.StringMap.t

  and binding = TypedBinding of t | Binding of Untyped.t

  and syntax =
      Void
    | Here
    | Select of (t * select)
    | Navigate of t * nav
    | First of t
    | Count of t
    | Screen of (t * screen)
    | Const of Const.t
    | Where of (t * Untyped.args)
    | Name of (string * t)
    | Locate of (t * t)
    | Meta of t
    | Grow of (t * t)
    | Compare of binary

  and binary = { op : Operator.t; left : t; right : t}

  and nav = { navName : string; }

  and screen = { screenName : string; screenArgs : Untyped.args; }

  and select = field list

  and field = { alias : string option; query : t; }

  val stripTypes : t -> Untyped.t

  val show : t -> string

  module Context : sig
    type t = context

    val void : t

    val bindings : t -> scope

    val addBindings : bindings:scope -> t -> t
  end

  val void : t

  val ctyp : t -> Type.ctyp

  val card : t -> Card.t

  val typ : t -> Type.t
end
