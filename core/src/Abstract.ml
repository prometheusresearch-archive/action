(**
 * This module contains abstract interface declarations for the action.
 *)

(**
 * Database
 *)
module type DATABASE = sig

  (** Abstract type which represents a database instance/handle *)
  type t

  (** Errors in which database computations could result *)
  type error = [ `DatabaseError of string | QueryTyper.error ]

  (** Computations performed with the database *)
  type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

  (**
   * Universe associated with the database.
   *)
  val univ : t -> Universe.t

  (**
   * Root value of the database.
   *)
  val root : t -> Value.t

  (**
   * Execute typed query.
   *)
  val query :
    ?value : Value.t
    -> db : t
    -> Query.Typed.t
    -> (Value.t, 'err) comp

end

(**
 * Workflow interpreter over the DB : DATABASE implementation.
 *)
module type RUN_WORKFLOW = functor (DB : DATABASE) -> sig

  (**
   * This type represents workflow execution state.
   *)
  type t

  type error = [
    `RunWorkflowError of string
  | `DatabaseError of string
  | `QueryTypeError of string
  ]

  type ('v, 'err) comp = ('v,  [> error ] as 'err) Run.t

  (**
   * Produce an initial state for the given database and workflow description.
   *)
  val boot : db : DB.t -> Workflow.Typed.t -> ((t * Value.UI.t option), 'err) comp

  (**
   * Render workflow state and return a new state and a UI screen to render.
   *)
  val render : t -> ((t * Value.UI.t option), 'err) comp

  val uiQuery : t -> (Query.Typed.t, 'err) comp

  val setArgs : args : Query.Untyped.args -> t -> (t, 'err) comp

  val step : t -> (t, 'err) comp

  val show : t -> string

  val breadcrumbs : t -> t list

  val next : t -> (t list, 'err) comp

end
