module Make (Universe : Abstract.UNIVERSE) : sig

  type error = [ `WorkflowTypeError of string | `QueryTypeError of string ]

  type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

  val typeWorkflow : univ : Universe.t -> Workflow.Untyped.t -> (Workflow.Typed.t, 'err) comp
end

