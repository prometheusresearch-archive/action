module StringMap = Common.StringMap

type t = op StringMap.t

and op =
  | Update of Query.Untyped.t
  | UpdateEntity of t
  | CreateEntity of t


module Syntax = struct

  type opSyntax = string * op

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

