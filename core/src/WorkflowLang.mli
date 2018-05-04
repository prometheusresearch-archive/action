(**
 * Workflow language.
 *)

(**
 * An workflow over some monoidal domain.
 *
 * A workflow is a set of named grammatical constructs (we call them nodes),
 * each node can be one of
 *
 * - Value (from a monoidal domain)
 * - Label (which points to another node in the workflow)
 * - Navigate (preprend a value before another node)
 * - Sequence (sequential composition of multiple nodes)
 * - Choice (parallel composition of multiple nodes)
 *
 *)
module Make (M : Abstract.MONOID) : sig

  (** Workflow *)
  type t

  and workflow = t

  (** A single node of the workflow *)
  and node

  (**
   * Convenience for creating workflows programmatically.
   *
   * Example
   *
   *  Workflow.Syntax(
   *    empty
   *    |> define "main" (
   *         seq [...; label "then"]
   *       )
   *    |> define "then" (
   *         par [...]
   *       )
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
    val value : M.t -> node

    (**
     * Construct a new workflow node which references another workflow node.
     *)
    val label : string -> node

    (**
     * Construct a new workflow node which multiples an existent node on a
     * value.
     *)
    val navigateAnd : M.t -> node -> node

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
    val start : ?value : M.t -> label : string -> workflow -> (t, string) Run.t

    (**
     * Find all next positions which contain some value and accumulate value
     * during the search.
     *)
    val next : t -> ((M.t * t) list, string) Run.t

  end

end

