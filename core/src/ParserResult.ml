type t =
  | Workflow of QueryWorkflow.t
  | Query of Query.Untyped.t
