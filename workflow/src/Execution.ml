module Query = struct
  type t
end

module UI = struct
  type t
end

module Value = struct
  type t
end

module DataSet : sig
  type t
  val empty : t
end = struct
  type t = Value.t Js.Dict.t
  let empty = Js.Dict.empty ()
end

module Context : sig
  type t

  module ContextType : sig
    type t
    module JS : sig
      val entity : string -> t
      val string : t
      val number : t
    end
  end

  module ContextValue : sig
    type t

    val value : t -> Value.t
    val ofType : t -> ContextType.t

    module JS : sig
      val entity : string -> Value.t -> t
      val string : Value.t -> t
      val number : Value.t -> t
    end
  end

  module Shape : sig
    type t

    val empty : t

    module JS : sig
      val ofJS : ContextType.t Js.Dict.t -> t
    end
  end

  val empty : t

  val matches : t -> Shape.t -> bool

  module JS : sig
    type jst
    val ofJS : jst -> t
    val toJS : t -> jst
  end

end = struct

  module ContextType = struct

    type t =
      | EntityType of string * t option
      | ScalarType of string

    let matches suptyp typ =
      match suptyp, typ with
      | EntityType _, ScalarType _
      | ScalarType _, EntityType _ -> false
      | ScalarType name, ScalarType oname -> name = oname
      | EntityType (name, None), EntityType (oname, None) -> name = oname
      | EntityType _, EntityType _ -> failwith "EntityType subtyping is not implemented"

    module JS = struct
      let entity name = EntityType (name, None)
      let string = ScalarType "string"
      let number = ScalarType "number"
    end

  end

  module ContextValue = struct
    type t = < value : Value.t; ofType : ContextType.t > Js.t

    let value v = v##value
    let ofType v = v##ofType

    module JS = struct
      let entity name v =
        let ofType = ContextType.EntityType (name, None) in
        [%bs.obj {ofType; value = v}]

      let string v =
        let ofType = ContextType.ScalarType "string" in
        [%bs.obj {ofType; value = v}]

      let number v =
        let ofType = ContextType.ScalarType "number" in
        [%bs.obj {ofType; value = v}]
    end
  end

  module StringMap = Map.Make(String)

  module Shape = struct
    type t = ContextType.t StringMap.t
    let empty = StringMap.empty

    module JS = struct
      let ofJS dict =
        let f shape (k, v) = StringMap.add k v shape in
        Js.Array.reduce f StringMap.empty (Js.Dict.entries dict)
    end
  end

  type t = ContextValue.t StringMap.t

  let empty = StringMap.empty

  let matches (context : t) (shape : Shape.t) =
    let f k v = function
      | false -> false
      | true -> begin match StringMap.find k context with
        | cv -> ContextType.matches (ContextValue.ofType cv) v
        | exception Not_found -> false
        end
    in
    StringMap.fold f shape true

  module JS = struct
    type jst = ContextValue.t Js.Dict.t

    let ofJS (dict : jst) =
      let f context (k, v) = StringMap.add k v context in
      Js.Array.reduce f StringMap.empty (Js.Dict.entries dict)

    let toJS (context : t) =
      let r: jst = Js.Dict.empty () in
      let f k v = Js.Dict.set r k v in
      StringMap.iter f context;
      r
  end
end

(** Primitive actions *)
module Action = struct

  type interaction = {
    requires : Context.Shape.t;
    provides : Context.Shape.t;
    query : Context.JS.jst -> Query.t;
    ui : UI.t;
  }

  type query = {
    requires : Context.Shape.t;
    provides : Context.Shape.t;
    query : Context.JS.jst -> Query.t;
  }

  type mutation = {
    requires : Context.Shape.t;
    provides : Context.Shape.t;
    query : Context.JS.jst -> Query.t;
    execute : Context.JS.jst -> DataSet.t -> Context.t;
  }

  type guard = {
    requires : Context.Shape.t;
    query : Context.JS.jst -> Query.t;
    check : Context.JS.jst -> DataSet.t -> bool;
  }

  type t =
    | Interaction of interaction
    | Query of query
    | Mutation of mutation
    | Guard of guard


end

(** Composition of actions *)
module Workflow = struct

  type t =
    | Sequence of t list
    | Choice of t list
    | Action of Action.t

end

module Frame = struct

  type t =
    | SequenceFrame of (info * Workflow.t list)
    | ChoiceFrame of (info * Workflow.t list)
    | ActionFrame of (info * Action.t)

  and info = {
    parent : t option;
    context : Context.t;
  }

  let make ?parent ?(context=Context.empty) (node : Workflow.t) =
    match node with
    | Workflow.Sequence actions ->
      SequenceFrame ({parent; context;}, actions)
    | Workflow.Choice actions ->
      ChoiceFrame ({parent; context;}, actions)
    | Workflow.Action action ->
      ActionFrame ({parent; context;}, action)

  let info (frame : t) =
    match frame with
    | SequenceFrame (info, _)
    | ChoiceFrame (info, _)
    | ActionFrame (info, _) -> info

  let updateInfo info (frame : t) =
    match frame with
    | SequenceFrame (_, v) -> SequenceFrame (info, v)
    | ChoiceFrame (_, v) -> ChoiceFrame (info, v)
    | ActionFrame (_, v) -> ActionFrame (info, v)

  let context (frame : t) =
    let {context; _} = info frame in
    context

  let updateContext context frame =
    let info = info frame in
    let info = { info with context; } in
    updateInfo info frame

end

module Execution = struct

  module Config = struct
    type t = {
      workflow : Workflow.t;
      context : Context.t;
      waitForData : Query.t -> DataSet.t Js.Promise.t;
    }
  end

  type t = {
    config : Config.t;
    frame : Frame.t;
  }

  let make (config : Config.t) =
    let frame =
      Frame.make
        ~context:config.context
        config.workflow
    in {
      frame;
      config;
    }

  let fetch (action : Action.t) (frame : Frame.t) (exec : t) =
    let query action =
      match action with
      | Action.Interaction { query; _ }
      | Action.Query { query; _ }
      | Action.Guard { query; _ } ->
        let context = Frame.context frame in
        Some (query (Context.JS.toJS context))
      | Action.Mutation _ -> None
    in match query action with
    | None -> Promise.return DataSet.empty
    | Some query -> exec.config.waitForData query


  let rec nextInSequence (frame : Frame.t) =
    let {Frame. parent; _} = Frame.info frame in
    match parent, frame with
    | _, Frame.SequenceFrame (info, (_cur::next)) ->
      let frame = Frame.SequenceFrame (info, next) in
      Some frame
    | Some parent, _ ->
      let parent = Frame.updateContext (Frame.context frame) parent in
      nextInSequence parent
    | None, _ -> None

  let rec nextInChoice (frame : Frame.t) =
    let {Frame. parent; _} = Frame.info frame in
    match parent, frame with
    | Some parent, Frame.ChoiceFrame (_, []) ->
      nextInSequence parent
    | _, Frame.ChoiceFrame (info, _cur::next) ->
      let frame = Frame.ChoiceFrame (info, next) in
      Some frame
    | Some parent, _ -> nextInChoice parent
    | None, _ -> None

  let speculate (exec : t) =
    let open Promise.Syntax in
    let {frame; _} = exec in

    let rec aux frame =
      match frame with

      | Frame.SequenceFrame (_, []) ->
        return []
      | Frame.SequenceFrame (_, action::_rest) ->
        let frame = Frame.make ~parent:frame action in
        aux frame

      | Frame.ChoiceFrame (_, []) ->
        return []
      | Frame.ChoiceFrame (_, actions) ->
        let%bind results =
          let f action =
            let frame = Frame.make ~parent:frame action in
            aux frame
          in
          Promise.all (ListLabels.map ~f actions)
        in
        return (List.concat results)

      | Frame.ActionFrame (_, Action.Mutation _) -> return []

      | Frame.ActionFrame (_, Action.Query _) -> failwith "Query is not implemented"

      | Frame.ActionFrame ({ context; _ }, Action.Interaction { requires; _ }) ->
        if Context.matches context requires
        then return [frame]
        else return []

      | Frame.ActionFrame ({ context; _ }, (Action.Guard { check; requires; _ } as action)) ->
        if Context.matches context requires
        then (
          let%bind data = fetch action frame exec in
          let allowed = check (Context.JS.toJS context) data in
          if allowed
          then nextOf frame
          else return []
        )
        else return []

    and nextOf frame = match nextInSequence frame with
    | None -> return []
    | Some frame -> aux frame

    in nextOf frame

  let run ~action (exec : t) =
    let open Promise.Syntax in

    let rec aux frame =
      let context = Frame.context frame in
      match frame with
      | Frame.SequenceFrame (_, cur::_rest) ->
        let frame = Frame.make ~context ~parent:frame cur in
        aux frame
      | Frame.SequenceFrame (_, []) ->
        nextOf frame
      | Frame.ActionFrame (_, (Action.Interaction ({ requires; _ } as interaction) as action)) ->
        if Context.matches context requires
        then
          let%bind data = fetch action frame exec in
          return (Some (context, data, interaction), frame)
        else bailOf frame
      | Frame.ActionFrame (_, (Action.Guard { requires; check; _ } as action)) ->
        if Context.matches context requires
        then (
          let%bind data = fetch action frame exec in
          let allowed = check (Context.JS.toJS context) data in
          if allowed
          then nextOf frame
          else bailOf frame
        )
        else bailOf frame

      | Frame.ActionFrame (_, Action.Query _) -> failwith "Query is not implemented"
      | Frame.ActionFrame (_, Action.Mutation _) -> failwith "Mutation is not implemented"

      | Frame.ChoiceFrame (_, cur::_rest) ->
        let frame = Frame.make ~context ~parent:frame cur in
        aux frame
      | Frame.ChoiceFrame (_, []) ->
        bailOf frame

    and bailOf frame =
      match nextInChoice frame with
      | Some frame -> aux frame
      | None -> return (None, frame)

    and nextOf frame =
      match nextInSequence frame with
      | Some frame ->

          aux frame
      | None -> return (None, frame)

    in

    let%bind res, nextFrame = match action with
    | `This -> aux exec.frame
    | `Next context ->
      let frame = Frame.updateContext context exec.frame in
      nextOf frame
    in return (res, { exec with frame = nextFrame })

end

(**
 * JS API
 *)
module JS = struct

  let context = Context.JS.ofJS
  let number = Context.ContextValue.JS.number
  let string = Context.ContextValue.JS.string
  let entity = Context.ContextValue.JS.entity

  let numberType = Context.ContextType.JS.number
  let stringType = Context.ContextType.JS.string
  let entityType = Context.ContextType.JS.entity

  let interaction params =
    Action.Interaction {
      Action.
      requires = Context.Shape.JS.ofJS params##requires;
      provides = Context.Shape.JS.ofJS params##provides;
      query = params##query;
      ui = params##ui;
    }

  let guard params =
    let check context data = (params##check context data) in
    Action.Guard {
      Action.
      requires = Context.Shape.JS.ofJS params##requires;
      query = params##query;
      check;
    }

  let action action =
    Workflow.Action action

  let sequence actions =
    let actions = actions |> Array.to_list in
    Workflow.Sequence actions

  let choice actions =
    let actions = actions |> Array.to_list in
    Workflow.Choice actions

  let make params =
    Execution.make {
      workflow = params##workflow;
      context = Context.empty;
      waitForData = params##waitForData;
    }

  let runToInteraction exec =
    let open Promise.Syntax in
    match%bind Execution.run ~action:`This exec with
    | Some (context, data, interaction), executor ->
      let context = Context.JS.toJS context in
      let result = (Js.Nullable.return [%bs.obj {context; data; ui = interaction.ui}]) in
      return [%bs.obj {executor; result}]
    | None, executor -> return [%bs.obj {executor; result = (Js.Nullable.null)}]

  let nextToInteraction context exec =
    let open Promise.Syntax in
    let context = Context.JS.ofJS context in
    match%bind Execution.run ~action:(`Next context) exec with
    | Some (context, data, interaction), executor ->
      let context = Context.JS.toJS context in
      let result = Js.Nullable.return [%bs.obj {context; data; ui = interaction.ui}] in
      return [%bs.obj {executor; result}]
    | None, executor ->
      let result = Js.Nullable.null in
      return [%bs.obj {executor; result}]

end

include JS
