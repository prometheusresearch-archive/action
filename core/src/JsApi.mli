(**
 * JS API
 *
 * This works as the facade for the Action API for JS consumers.
 *)

(** Database *)
type db

(** Workflow Configuration *)
type workflow

(** Workflow state *)
type state

(** UI *)
type ui

(** Value *)
type value

(** Query *)
type query = string

(** Args *)
type args = Js.Json.t

val db : db

(** Run [workflow] given the [db] *)
val run : db -> workflow -> state

(** Get a list of next possible state given the current state. *)
val next : state -> state array

(** Get a breadcrumb for the current state. *)
val breadcrumb : state -> state array

(** Update [state] with [args]. *)
val replaceArgs : args -> state -> state

(** Get [ui] for the [state] *)
val ui : state -> ui

(** Get [ui] for the [state] *)
val query : query -> state -> value

(** Parse [workflow] config. *)
val parseWorkflow : string -> workflow
