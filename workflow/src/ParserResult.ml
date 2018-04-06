type t =
  | Workflow of Workflow.Untyped.t
  | Query of Core.Query.t
