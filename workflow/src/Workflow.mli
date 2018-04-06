module Untyped : sig

  type t =
    (** Render concrete query to a screen *)
    | Render of Core.Query.t
    (** Define how to transition from one screen to another screen *)
    | Next of (t * t list)

  module Syntax : sig
    val render : Core.Query.t -> t
    val andThen : t list -> t -> t
  end
end

module Typed : sig

  type t =
    (** Render concrete query to a screen *)
    | Render of Core.Query.t
    (** Define how to transition from one screen to another screen *)
    | Next of (t * t list)

end

module Typer : sig

  type error = [ `WorkflowTypeError of string ]
  type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

  val typeWorkflow : univ : Core.Universe.t -> Untyped.t -> (Typed.t, 'err) comp
end
