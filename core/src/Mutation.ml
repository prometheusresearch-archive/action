module StringMap = Common.StringMap

module Untyped = struct
  type t = op StringMap.t

  and op =
    | Update of Query.Untyped.t
    | UpdateEntity of t
    | CreateEntity of t


  module Syntax = struct

    type nonrec op = string * op

    let update ~name q = name, Update q

    let rec updateEntity ~name mutSyn =
      let mut = mutation mutSyn in
      name, UpdateEntity mut

    and createEntity ~name mutSyn =
      let mut = mutation mutSyn in
      name, CreateEntity mut

    and mutation ops =
      let f map (name, op) = StringMap.set map name op in
      Belt.List.reduce ops StringMap.empty f
  end
end

module Typed = struct
  type t = op StringMap.t

  and op =
    | Update of Query.Untyped.t
    | UpdateEntity of t
    | CreateEntity of t

end

module Typer = struct

  type error = [ `QueryTypeError of string ]

  type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

  let rec typeMutation ~univ ~query mut =
    let open Run.Syntax in
    let module Q = Query.Untyped.Syntax in
    let typeOp map key op =
      let%bind op = match op with
      | Untyped.Update q ->
        let%bind _ = QueryTyper.growQuery ~univ ~base:query Q.(here |> nav key) in
        let%bind _ = QueryTyper.growQuery ~univ ~base:query q in
        (* TODO: check that types can unified from both nav and value *)
        return (Typed.Update q)
      | Untyped.UpdateEntity mut ->
        let%bind query = QueryTyper.growQuery ~univ ~base:query Q.(here |> nav key) in
        let%bind mut = typeMutation ~univ ~query mut in
        return (Typed.UpdateEntity mut)
      | Untyped.CreateEntity mut ->
        let%bind query = QueryTyper.growQuery ~univ ~base:query Q.(here |> nav key) in
        let%bind mut = typeMutation ~univ ~query mut in
        return (Typed.CreateEntity mut)
      in
      return (StringMap.set map key op)
    in
    Run.StringMap.foldLeft ~f:typeOp ~init:StringMap.empty mut

end
