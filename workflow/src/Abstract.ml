(**
 * This module contains abstract interface declarations for the action.
 *)

open Core

(**
 * Database
 *)
module type DATABASE = sig

  type t
  type error = [ `DatabaseError of string ]
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
  val execute :
    ?value : Value.t
    -> db : t
    -> TypedQuery.t
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

  type error = [ `RunWorkflowError of string | `DatabaseError of string ]
  type ('v, 'err) comp = ('v,  [> error ] as 'err) Run.t

  (**
   * Produce an initial state for the given database and workflow description.
   *)
  val boot : db : DB.t -> TypedWorkflow.t -> ((t * Value.UI.t option), 'err) comp

  (**
   * Render workflow state and return a new state and a UI screen to render.
   *)
  val render : t -> ((t * Value.UI.t option), 'err) comp

  val dataQuery : t -> (TypedQuery.t, 'err) comp

  val titleQuery : t -> (TypedQuery.t, 'err) comp

  val uiQuery : t -> (TypedQuery.t, 'err) comp

  val setArgs : args : Query.args -> t -> (t, 'err) comp

  val step : t -> (t, 'err) comp

  val show : t -> string

  val breadcrumbs : t -> t list

  val next : t -> (t list, 'err) comp

end
