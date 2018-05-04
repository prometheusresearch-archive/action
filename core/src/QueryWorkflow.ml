module Workflow = WorkflowLang.Make(struct
  type t = Query.Untyped.t

  let empty = Query.Untyped.Syntax.void
  let append = Query.Untyped.Syntax.grow
  let show = Query.Untyped.show
end)

include Workflow
