(**
 * Workflow language.
 *)

(**
 * A workflow.
 *
 * A workflow is a set of named grammatical constructs (we call them nodes),
 * each node can be one of
 *
 * - value (from a monoidal value)
 * - label (which points to another node in the workflow)
 * - navigate (preprend a value before another node)
 * - sequence (sequential composition of multiple nodes)
 * - choice (parallel composition of multiple nodes)
 *
 *)
module type Lang = sig

  (** Workflow *)
  type t

  and workflow = t

  (** A single node of the workflow *)
  and node

  and value

  (**
   * Convenience for creating workflows programmatically.
   *
   * Example
   *
   *  Lang.Syntax(
   *    empty
   *    |> define "main" (
   *         seq [...; label "then"]
   *       )
   *    |> define "then" (
   *         par [...]
   *       )
   *  )
   *
   *)
  module Syntax : sig

    (** An empty workflow *)
    val empty : t

    (**
     * Add a new labelled node to the workflow.
     *
     * Label is used by label combinator to reference that workflow later.
     *)
    val define : string -> node -> t -> t

    (**
     * Construct a new workflow node out of value.
     *)
    val value : value -> node

    (**
     * Construct a new workflow node which references another workflow node.
     *)
    val label : string -> node

    (**
     * Construct a new workflow node which multiples an existent node on a
     * value.
     *)
    val navigateAnd : value -> node -> node

    (**
     * A sequential composition of workflow nodes.
     *)
    val seq : node list -> node

    (**
     * A parallel composition of workflow nodes.
     *)
    val par : node list -> node

  end

  (**
   * Position inside workflow.
   *)
  module Pos : sig

    type t

    (**
     * Construct a start position
     *)
    val start : ?value : value -> label : string -> workflow -> (t, string) Run.t

    (**
     * Find all next positions which contain some value and accumulate value
     * during the search.
     *)
    val next : t -> ((value * t) list, string) Run.t

  end
end

(**
 * Construct workflow out of monoidal structure.
 *)
module Make (M : Abstract.MONOID) : Lang with type value := M.t
