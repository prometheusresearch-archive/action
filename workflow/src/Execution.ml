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

type js
external js : 'a -> js = "%identity"

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
  end

  val empty : t

  val matches : t -> Shape.t -> bool

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

  module Shape = struct
    type t = ContextType.t Js.Dict.t
    let empty = Js.Dict.empty ()
  end

  type t = ContextValue.t Js.Dict.t

  let empty = Js.Dict.empty ()

  let matches (context : t) (shape : Shape.t) =
    let f prev (k, v) = match prev with
      | false -> false
      | true -> begin match Js.Dict.get context k with
        | Some cv -> ContextType.matches (ContextValue.ofType cv) v
        | None -> false
        end
    in
    shape |> Js.Dict.entries |> Js.Array.reduce f true

end

(** Primitive actions *)
module Action = struct

  type interaction = {
    requires : Context.Shape.t;
    provides : Context.Shape.t;
    query : Context.t -> Query.t;
    ui : UI.t;
  } [@@bs.deriving jsConverter]

  type query = {
    requires : Context.Shape.t;
    provides : Context.Shape.t;
    query : Context.t -> Query.t;
  } [@@bs.deriving jsConverter]

  type mutation = {
    requires : Context.Shape.t;
    provides : Context.Shape.t;
    query : Context.t -> Query.t;
    execute : Context.t -> DataSet.t -> Context.t;
  } [@@bs.deriving jsConverter]

  type guard = {
    requires : Context.Shape.t;
    query : Context.t -> Query.t;
    check : Context.t -> DataSet.t -> bool;
  } [@@bs.deriving jsConverter]

  type t =
    | Interaction of interaction
    | Query of query
    | Mutation of mutation
    | Guard of guard

  let toJs = function
    | Interaction v ->
      js [%bs.obj {action = "Interaction"; interaction = js (interactionToJs v)}]
    | Query v ->
      js [%bs.obj {action = "Interaction"; query = js (queryToJs v)}]
    | Mutation v ->
      js [%bs.obj {action = "Interaction"; mutation = js (mutationToJs v)}]
    | Guard v ->
      js [%bs.obj {action = "Interaction"; guard = js (guardToJs v)}]

end

(** Sequential and parallel composition of actions *)
module Node = struct

  type t =
    | Sequence of t list
    | Choice of t list
    | Action of Action.t

  let rec toJs = function
  | Sequence v ->
    let v = v |> List.map toJs |> Array.of_list in
    js [%bs.obj {node = "Sequence"; sequence = js v }]
  | Choice v ->
    let v = v |> List.map toJs |> Array.of_list in
    js [%bs.obj {node = "Choice"; choice = js v }]
  | Action v ->
    let v = Action.toJs v in
    js [%bs.obj {node = "Action"; action = js v }]

end

module Frame = struct

  type t = (info * pos)

  and pos =
    | SequenceFrame of Node.t list
    | ChoiceFrame of Node.t list
    | ActionFrame of actionInfo

  and info = {
    parent : t option;
    context : Context.t;
  }

  and actionInfo = {
    action: Action.t;
    prev: (Action.t * t) option;
  }

  let rec actionInfoToJs info =
    let prev = match info.prev with
    | Some (action, frame) ->
      Js.Nullable.return [%bs.obj { action = Action.toJs action; frame = toJs frame }]
    | None -> Js.Nullable.null
    in
    [%bs.obj {action = Action.toJs info.action; prev}]

  and infoToJs info =
    let parent = match info.parent with
    | Some parent ->
      Js.Nullable.return (toJs parent)
    | None -> Js.Nullable.null
    in
    [%bs.obj {parent}]

  and posToJs = function
  | SequenceFrame v ->
    let v = v |> List.map Node.toJs |> Array.of_list in
    js [%bs.obj {frame = "SequenceFrame"; sequence = js v }]
  | ChoiceFrame v ->
    let v = v |> List.map Node.toJs |> Array.of_list in
    js [%bs.obj {frame = "ChoiceFrame"; choice = js v }]
  | ActionFrame v ->
    js [%bs.obj {frame = "ActionFrame"; action = js (actionInfoToJs v) }]

  and toJs (info, pos) =
    [%bs.obj {info = infoToJs info; pos = posToJs pos}]

  let make ?prev ?parent ?(context=Context.empty) (node : Node.t) =
    let info = {parent; context} in
    match node with
    | Node.Sequence actions ->
      info, SequenceFrame actions
    | Node.Choice actions ->
      info, ChoiceFrame actions
    | Node.Action action ->
      info, ActionFrame {action; prev}

  let context (frame : t) =
    let {context; _}, _ = frame in
    context

  let updateContext context frame =
    let info, pos = frame in
    let info = { info with context; } in
    info, pos

end

module Execution = struct

  type config = {
    waitForData : Query.t -> DataSet.t Js.Promise.t;
  }

  let init workflow =
    Frame.make workflow

  let fetch (action : Action.t) (frame : Frame.t) (config : config) =
    let query action =
      match action with
      | Action.Interaction { query; _ }
      | Action.Query { query; _ }
      | Action.Guard { query; _ } ->
        let context = Frame.context frame in
        Some (query context)
      | Action.Mutation _ -> None
    in match query action with
    | None -> Promise.return DataSet.empty
    | Some query -> config.waitForData query


  let rec nextInSequence ~config (frame : Frame.t) =
    let {Frame. parent; _}, _ = frame in
    match parent, frame with
    | _, (info, Frame.SequenceFrame (_cur::next)) ->
      let frame = info, Frame.SequenceFrame next in
      Some frame
    | Some parent, _ ->
      let parent = Frame.updateContext (Frame.context frame) parent in
      nextInSequence ~config parent
    | None, _ -> None

  let rec nextInChoice ~config (frame : Frame.t) =
    let {Frame. parent; _}, _ = frame in
    match parent, frame with
    | Some parent, (_, Frame.ChoiceFrame []) ->
      nextInSequence ~config parent
    | _, (info, Frame.ChoiceFrame (_cur::next)) ->
      let frame = info, Frame.ChoiceFrame next in
      Some frame
    | Some parent, _ -> nextInChoice ~config parent
    | None, _ -> None

  (*
  let speculate (exec : t) =
    let open Promise.Syntax in
    let {frame; _} = exec in

    let rec aux frame =
      match frame with

      | _, Frame.SequenceFrame [] ->
        return []
      | _, Frame.SequenceFrame (action::_rest) ->
        let frame = Frame.make ~parent:frame action in
        aux frame

      | _, Frame.ChoiceFrame [] ->
        return []
      | _, Frame.ChoiceFrame actions ->
        let%bind results =
          let f action =
            let frame = Frame.make ~parent:frame action in
            aux frame
          in
          Promise.all (ListLabels.map ~f actions)
        in
        return (List.concat results)

      | _, Frame.ActionFrame { action = Action.Mutation _; _} -> return []

      | _, Frame.ActionFrame { action = Action.Query _; _ } -> failwith "Query is not implemented"

      | {Frame. context; _}, Frame.ActionFrame { action = Action.Interaction { requires; _ }; _} ->
        if Context.matches context requires
        then return [frame]
        else return []

      | {Frame. context; _}, Frame.ActionFrame {
          action = Action.Guard { check; requires; _ } as action; _
        } ->
        if Context.matches context requires
        then (
          let%bind data = fetch action frame exec in
          let allowed = check context data in
          if allowed
          then nextOf frame
          else return []
        )
        else return []

    and nextOf frame = match nextInSequence frame with
    | None -> return []
    | Some frame -> aux frame

    in nextOf frame
  *)

  let run ~action ~config (currentFrame : Frame.t) =
    let open Promise.Syntax in

    let rec runToInteraction ~prev frame =
      let context = Frame.context frame in
      match frame with
      | _, Frame.SequenceFrame (cur::_rest) ->
        let frame = Frame.make ?prev ~context ~parent:frame cur in
        runToInteraction ~prev frame
      | _, Frame.SequenceFrame [] ->
        nextOf ~prev frame
      | _, Frame.ActionFrame {
          action = Action.Interaction ({ requires; _ } as interaction) as action;
          _
        } ->
        if Context.matches context requires
        then
          let%bind data = fetch action frame config in
          return (Some (context, data, interaction), frame)
        else bailOf ~prev frame
      | _, Frame.ActionFrame {
          action = Action.Guard { requires; check; _ } as action;
          _
        } ->
        if Context.matches context requires
        then (
          let%bind data = fetch action frame config in
          let allowed = check context data in
          if allowed
          then nextOf ~prev frame
          else bailOf ~prev frame
        )
        else bailOf ~prev frame

      | _, Frame.ActionFrame { action = Action.Query _; _ } -> failwith "Query is not implemented"
      | _, Frame.ActionFrame { action = Action.Mutation _; _ } -> failwith "Mutation is not implemented"

      | _, Frame.ChoiceFrame (cur::_rest) ->
        let frame = Frame.make ?prev ~context ~parent:frame cur in
        runToInteraction ~prev frame
      | _, Frame.ChoiceFrame [] ->
        bailOf ~prev frame

    and bailOf ~prev frame =
      match nextInChoice ~config frame with
      | Some frame -> runToInteraction ~prev frame
      | None -> return (None, frame)

    and nextOf ~prev frame =
      match nextInSequence ~config frame with
      | Some frame -> runToInteraction ~prev frame
      | None -> return (None, frame)

    in

    let prev = match currentFrame with
    | (_,Frame.ActionFrame { action; _ }) -> Some (action, currentFrame)
    | (_,Frame.SequenceFrame _)
    | (_,Frame.ChoiceFrame _) -> None
    in

    let frame = match action with
      | `This -> Some currentFrame
      | `Next context->
        currentFrame
        |> Frame.updateContext context
        |> nextInSequence ~config
    in

    match frame with
    | Some frame ->
      let%bind res, nextFrame = match action with
      | `This ->
        runToInteraction ~prev frame
      | `Next context ->
        let frame = Frame.updateContext context currentFrame in
        nextOf ~prev frame
      in return (res, nextFrame)
    | None -> return (None, currentFrame)

end

(**
 * JS API
 *)
module JS = struct

  let number = Context.ContextValue.JS.number
  let string = Context.ContextValue.JS.string
  let entity = Context.ContextValue.JS.entity

  let numberType = Context.ContextType.JS.number
  let stringType = Context.ContextType.JS.string
  let entityType = Context.ContextType.JS.entity

  let interaction params =
    Action.Interaction {
      Action.
      requires = params##requires;
      provides = params##provides;
      query = params##query;
      ui = params##ui;
    }

  let guard params =
    let check context data = (params##check context data) in
    Action.Guard {
      Action.
      requires = params##requires;
      query = params##query;
      check;
    }

  let action action =
    Node.Action action

  let sequence actions =
    let actions = actions |> Array.to_list in
    Node.Sequence actions

  let choice actions =
    let actions = actions |> Array.to_list in
    Node.Choice actions

  let init = Execution.init

  let trace currentFrame =
    let rec collectPrevFrames acc frame =
      match frame with
      | _, Frame.ActionFrame { prev = Some (Action.Interaction {ui;_}, prevFrame); _ } ->
        let r = [%bs.obj { ui; frame = prevFrame }] in
        collectPrevFrames (r::acc) prevFrame
      | { Frame. parent = Some parentFrame }, _ ->
        collectPrevFrames acc parentFrame
      | _ -> acc
    in
    currentFrame
    |> collectPrevFrames []
    |> List.rev
    |> Array.of_list

  let runToInteraction config currentFrame =
    let open Promise.Syntax in
    let config = { Execution. waitForData = config##waitForData } in
    match%bind Execution.run ~config ~action:`This currentFrame with
    | Some (context, data, interaction), frame ->
      let info = (Js.Nullable.return [%bs.obj {
        context;
        data;
        ui = interaction.ui;
        trace = trace frame;
      }]) in
      return [%bs.obj {frame; info}]
    | None, frame ->
      return [%bs.obj {frame; info = (Js.Nullable.null)}]

  let nextToInteraction config context currentFrame =
    let open Promise.Syntax in
    let config = { Execution. waitForData = config##waitForData } in
    match%bind Execution.run ~config ~action:(`Next context) currentFrame with
    | Some (context, data, interaction), frame ->
      let info = Js.Nullable.return [%bs.obj {
        context;
        data;
        ui = interaction.ui;
        trace = trace frame;
      }] in
      return [%bs.obj {frame; info}]
    | None, frame ->
      let info = Js.Nullable.null in
      return [%bs.obj {frame; info}]

end

include JS
