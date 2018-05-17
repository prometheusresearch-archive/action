type t =
  | Workflow of QueryWorkflow.Lang.t
  | Query of Query.Untyped.t
