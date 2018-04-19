module Result = Common.Result
module StringMap = Common.StringMap

module Untyped = struct
  type t =
    | Root
    | Render of render
    | AndThen of (t * t list)
    | Label of string

  and render = {
    query : Query.Untyped.t;
    label : string option;
  }

  module Syntax = struct
    let root = Root
    let render ?label query = Render {query; label}
    let andThen path node = AndThen (node, path)
    let label name = Label name
  end
end

module Typed = struct
  type t =
    | Root
    | Render of Untyped.render
    | AndThen of t * t list
    | Label of string
end
