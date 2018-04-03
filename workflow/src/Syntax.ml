

module StringMap = Belt.Map.String

module Query = struct
  type 'ctx t =
    | Void : 'ctx t
    | Navigate : 'ctx t * string -> 'ctx t
    | Where : 'ctx t * unit t StringMap.t -> 'ctx t
    | Name : string * 'ctx t -> 'ctx t

end

module UntypedQuery = struct
  type t = unit Query.t
end

module Type = struct
  type t = Number
end

module TypedQuery = struct
  type t = Type.t Query.t
end
