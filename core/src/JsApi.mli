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

(**
 * Run [workflow] given the [db]
 *)
val run : db -> workflow -> state

(**
 * Get a list of states next to the current [state].
 *)
val next : state -> state array

(**
 * Get a list of states around the current [state].
 *
 * This includes the current state.
 *)
val around : state -> state array

(**
 * Get a breadcrumb for the current [state].
 *)
val breadcrumb : state -> state array

(**
 * Update [state] with [args].
 *)
val replaceArgs : args -> state -> state

(**
 * Get [ui] for the [state]
 *)
val ui : state -> ui

(**
 * Run [query] against the context specified by the current [state].
 *)
val query : query -> state -> value

(**
 * Parse [workflow] out of a string.
 *)
val parseWorkflow : string -> workflow
