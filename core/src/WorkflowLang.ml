(**
 * Workflow language.
 *)

module Map = Belt.Map.String
module Set = Belt.Set.String
module List = Belt.List

(**
 * An workflow over some monoidal domain.
 *
 * A workflow is a set of named grammatical constructs (we call them nodes),
 * each node can be one of
 *
 * - Value (from a monoidal domain)
 * - Label (which points to another node in the workflow)
 * - Navigate (preprend a value before another node)
 * - Seq (sequential composition of multiple nodes)
 * - Par (parallel composition of multiple nodes)
 *
 *)
module Make (M : Abstract.MONOID) = struct

  type t = node Map.t

  and workflow = t

  and node =
    | Value of M.t
    | Label of string
    | NavigateAnd of M.t * node
    | Seq of node list
    | Par of node list

  module Syntax = struct

    let empty = Map.empty

    let define name node workflow =
      Map.set workflow name node

    let value v = Value v
    let label name = Label name
    let navigateAnd v node = NavigateAnd (v, node)
    let seq nodes = Seq nodes
    let par nodes = Par nodes
  end

  (**
   * Location inside node structure.
   *)
  module Loc : sig

    type t = node * ctx

    and ctx =
      | Root
      | InNavigateAnd of t * M.t
      | InSequence of t * node list * node list
      | InChoice of t * node list * node list
  end = struct

    type t = node * ctx

    and ctx =
      | Root
      | InNavigateAnd of t * M.t
      | InSequence of t * node list * node list
      | InChoice of t * node list * node list

  end

  (**
   * Position inside workflow.
   *)
  module Pos : sig

    type t = {
      (** Workflow *)
      workflow: workflow;
      (** Label inside the workflow *)
      label: string;
      (** Location inside node structure and a stack of prev locations *)
      loc: Loc.t * Loc.t list;
      (** Accumulated value *)
      value : M.t;
    }

    val start : ?value : M.t -> label : string -> workflow -> (t, string) Run.t
    val next : t -> ((M.t * t) list, string) Run.t

  end = struct

    type t = {
      workflow: workflow;
      label: string;
      loc: Loc.t * Loc.t list;
      value : M.t;
    }

    (**
     * Start the workflow given the label and optional start value.
     *)
    let start ?(value=M.empty) ~label workflow =
      let open Run.Syntax in
      match Map.get workflow label with
      | Some node ->
        return {
          workflow;
          label;
          value;
          loc = (node, Loc.Root), [];
        }
      | None ->
        error {j|unknown label $label|j}

    let next {loc; workflow; value; label} =
      let open Run.Syntax in

      let rec climbToNextInSequence ((curr, ctx), locs) =
        match ctx, locs with
        | Loc.Root, [] -> None
        | Loc.Root, prevLoc::stack -> climbToNextInSequence (prevLoc, stack)
        | Loc.InNavigateAnd (parent, _), _
        | Loc.InChoice (parent, _, _), _
        | Loc.InSequence (parent, _, []), _ ->
          climbToNextInSequence (parent, locs)
        | Loc.InSequence (parent, left, next::right), _ ->
          Some ((next, Loc.InSequence (parent, curr::left, right)), locs)
      in

      let recurseToValues loc =
        let rec aux value label visited acc ((curr, _ctx as loc), locs) =
          match curr with
          | Value locValue ->
            let value = M.append value locValue in
            let pos = {label; workflow; value; loc = loc, locs} in
            return ((value, pos)::acc)
          | Label nextLabel ->
            if Set.has visited nextLabel then
              return acc
            else begin
              let visited = Set.add visited nextLabel in
              match Map.get workflow nextLabel with
              | Some node ->
                let locs = loc::locs in
                let loc = node, Loc.Root in
                aux value nextLabel visited acc (loc, locs)
              | None ->
                error {j|no such label $nextLabel|j}
            end
          | NavigateAnd (query, node) ->
            let value = M.append value query in
            let loc = node, Loc.InNavigateAnd (loc, query) in
            aux value label visited acc (loc, locs)
          | Seq [] ->
            begin match climbToNextInSequence (loc, locs) with
            | Some loc -> aux value label visited acc loc
            | None -> return acc
            end
          | Seq nodes ->
            let rec f acc left = function
              | node::right ->
                let loc = node, Loc.InSequence (loc, left, right) in
                begin match%bind aux value label visited [] (loc, locs) with
                | [] -> f acc (node::left) right
                | lacc -> return (lacc @ acc)
                end
              | [] -> return acc
            in
            f acc [] nodes

          | Par nodes ->
            let rec f acc left = function
              | node::right ->
                let loc = node, Loc.InChoice (loc, left, right) in
                let%bind acc = aux value label visited acc (loc, locs) in
                f acc (node::left) right
              | [] -> return acc
            in
            nodes |> List.reverse |> f acc []

        in
        let visited = Set.(empty |. add label) in
        aux value label visited [] loc
      in

      (* climb to next in sequence if we are at the value *)
      let loc =
        match loc with
        | (Value _, Root), _ -> Some loc
        | (Value _, _), _ -> climbToNextInSequence loc
        | (Label _, _), _
        | (NavigateAnd _, _), _
        | (Seq _, _), _
        | (Par _, _), _ -> Some loc
      in

      match loc with
      | Some loc -> recurseToValues loc
      | None -> return []

  end

end
