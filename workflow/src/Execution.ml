module Query : sig
  type t

  val ofString : string -> t
end = struct
  type t = string

  let ofString query = query
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

module Inspect = struct

  type t
  external make : 'a -> t = "%identity"

  let id a = a

  let option inspect v = match v with
  | Some v -> Js.Nullable.return (inspect v)
  | None -> Js.Nullable.null

  let list inspect v =
    v |> List.map inspect |> Array.of_list

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

module Interaction = struct

  type t = {
    requires : Context.Shape.t;
    provides : Context.Shape.t;
    query : Context.t -> Query.t;
    queryTitle : Context.t -> Query.t option;
    ui : UI.t;
  } [@@bs.deriving jsConverter]

  let inspect = tToJs

end

(** Primitive actions *)
module Process = struct

  type query = {
    requires : Context.Shape.t;
    provides : Context.Shape.t;
    query : Context.t -> Query.t;
    update : Context.t -> DataSet.t -> Context.t;
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
    | Query of query
    | Mutation of mutation
    | Guard of guard

  let inspect = function
    | Query v ->
      Inspect.make [%bs.obj {action = "Query"; query = Inspect.make (queryToJs v)}]
    | Mutation v ->
      Inspect.make [%bs.obj {action = "Mutation"; mutation = Inspect.make (mutationToJs v)}]
    | Guard v ->
      Inspect.make [%bs.obj {action = "Guard"; guard = Inspect.make (guardToJs v)}]

end

(** Sequential and parallel composition of actions *)
module Node = struct

  type t =
    | Sequence of t list
    | Choice of t list
    | Process of Process.t
    | Interaction of Interaction.t

  let rec inspect = function
  | Sequence v ->
    let v = Inspect.(list inspect v) in
    Inspect.make [%bs.obj {node = "Sequence"; sequence = Inspect.make v }]
  | Choice v ->
    let v = Inspect.(list inspect v) in
    Inspect.make [%bs.obj {node = "Choice"; choice = Inspect.make v }]
  | Process v ->
    let v = Process.inspect v in
    Inspect.make [%bs.obj {node = "Process"; action = Inspect.make v }]
  | Interaction v ->
    let v = Interaction.inspect v in
    Inspect.make [%bs.obj {node = "Interaction"; action = Inspect.make v }]

end

module Frame = struct

  type t = (info * pos)

  and pos =
    | SequenceFrame of Node.t list
    | ChoiceFrame of Node.t list
    | InteractionFrame of interactionInfo
    | ProcessFrame of Process.t

  and interactionInfo = {
    interaction : Interaction.t;
    data : DataSet.t option;
    dataTitle : DataSet.t option;
    prev : (interactionInfo * t) option;
  }

  and info = {
    parent : t option;
    context : Context.t;
  }

  let make
      ?parent
      ?prev
      ?(context=Context.empty)
      (node : Node.t)
    =
    let info = {parent; context} in
    let frame = lazy (match node with
      | Node.Sequence actions ->
        info, SequenceFrame actions
      | Node.Choice actions ->
        info, ChoiceFrame actions
      | Node.Process process ->
        info, ProcessFrame process
      | Node.Interaction interaction ->
        info, InteractionFrame {
          interaction;
          data = None;
          dataTitle = None;
          prev;
        }
    ) in Lazy.force frame

  let context (frame : t) =
    let {context; _}, _ = frame in
    context

  let updateContext context frame =
    let info, pos = frame in
    let info = { info with context; } in
    info, pos

  let rec nextInSequence (frame : t) =
    let {parent; _}, _ = frame in
    match parent, frame with
    | _, (info, SequenceFrame (_cur::next)) ->
      let frame = info, SequenceFrame next in
      Some frame
    | Some parent, _ ->
      let parent = updateContext (context frame) parent in
      nextInSequence parent
    | None, _ ->
      None

  let rec nextInChoice (frame : t) =
    let {parent; _}, _ = frame in
    match parent, frame with
    | _, (info, ChoiceFrame (_cur::next)) ->
      let frame = info, ChoiceFrame next in
      Some frame
    | Some parent, _ ->
      nextInChoice parent
    | None, _ ->
      None

  let rec inspectInteractionFrame v =
    let interactionFrameAndFrameToJs (info, frame) =
      [%bs.obj {
        frame = inspect frame;
        info = inspectInteractionFrame info;
      }]
    in
    [%bs.obj {
      interaction = Interaction.inspect v.interaction;
      data = Inspect.option Inspect.make v.data;
      prev = Inspect.option interactionFrameAndFrameToJs v.prev;
    }]

  and inspectInfo v =
    let parent = match v.parent with
    | Some parent ->
      Js.Nullable.return (inspect parent)
    | None -> Js.Nullable.null
    in
    [%bs.obj {parent}]

  and inspectPos = function
  | SequenceFrame v ->
    let v = Inspect.(list Node.inspect v) in
    Inspect.make [%bs.obj {frame = "SequenceFrame"; sequence = Inspect.make v }]
  | ChoiceFrame v ->
    let v = Inspect.(list Node.inspect v) in
    Inspect.make [%bs.obj {frame = "ChoiceFrame"; choice = Inspect.make v }]
  | ProcessFrame v ->
    Inspect.make [%bs.obj {frame = "ProcessFrame"; action = Inspect.make (Process.inspect v) }]
  | InteractionFrame v ->
    Inspect.make [%bs.obj {frame = "InteractionFrame"; action = Inspect.make (inspectInteractionFrame v) }]

  and inspect (info, pos) =
    [%bs.obj {info = inspectInfo info; pos = inspectPos pos}]

end

module Execution = struct

  type config = {
    waitForData : Query.t -> DataSet.t Js.Promise.t;
  }

  let init workflow =
    Frame.make workflow

  let fetchFrameData ~config (frame : Frame.t) =
    let query = match frame with
      | _, Frame.SequenceFrame _
      | _, Frame.ChoiceFrame _ -> None
      | _, Frame.InteractionFrame { interaction = { query; _ }; }
      | _, Frame.ProcessFrame (Process.Query { query; _ })
      | _, Frame.ProcessFrame (Process.Mutation { query; _ })
      | _, Frame.ProcessFrame (Process.Guard { query; _ }) -> Some query
    in match query with
    | None -> Promise.return DataSet.empty
    | Some query ->
      let context = Frame.context frame in
      config.waitForData (query context)

  let fetchInteractionTitleData ~config ~context (interaction : Interaction.t) =
    let {Interaction. queryTitle; _} = interaction in
    match queryTitle context with
    | None -> Promise.return DataSet.empty
    | Some query -> config.waitForData query

  let interactionPointerOfFrame frame =
    match frame with
    | _, Frame.InteractionFrame info -> Some (info, frame)
    | _, Frame.ProcessFrame _
    | _, Frame.SequenceFrame _
    | _, Frame.ChoiceFrame _ -> None

  let next ~config (currentFrame : Frame.t) =
    let open Promise.Syntax in

    let prev = interactionPointerOfFrame currentFrame in

    let rec speculateToInteraction frame =
      let context = Frame.context frame in
      match frame with
      | _, Frame.SequenceFrame [] ->
        return []
      | _, Frame.SequenceFrame (action::_rest) ->
        let frame = Frame.make ?prev ~context ~parent:frame action in
        speculateToInteraction frame

      | _, Frame.ChoiceFrame [] ->
        return []
      | _, Frame.ChoiceFrame actions ->
        let%bind results =
          let f action =
            let frame = Frame.make ?prev ~context ~parent:frame action in
            speculateToInteraction frame
          in
          Promise.all (ListLabels.map ~f actions)
        in
        return (List.concat results)

      | _, Frame.ProcessFrame (Process.Mutation _) -> return []

      | _, Frame.ProcessFrame (Process.Query { requires; update; _ }) ->
        if Context.matches context requires
        then (
          let%bind data = fetchFrameData ~config frame in
          let context = update context data in
          let frame = Frame.updateContext context frame in
          nextOf frame
        )
        else return []

      | {Frame. context; _}, Frame.ProcessFrame (Process.Guard { check; requires; _ }) ->
        if Context.matches context requires
        then (
          let%bind data = fetchFrameData ~config frame in
          let allowed = check context data in
          if allowed
          then nextOf frame
          else return []
        )
        else return []

      | _, Frame.InteractionFrame info ->
        if Context.matches context info.interaction.requires
        then return [(info, frame)]
        else return []

    and nextOf frame = match Frame.nextInSequence frame with
    | None -> return []
    | Some frame -> speculateToInteraction frame

    in nextOf currentFrame

  let alternatives ~config (currentFrame : Frame.t) =
    match interactionPointerOfFrame currentFrame with
    | Some ({ prev = Some (_, prevFrame) }, _frame) -> next ~config prevFrame
    | _ -> Promise.return []

  let run ~action ~config (currentFrame : Frame.t) =
    let open Promise.Syntax in

    let frame = match action with
      | `This -> currentFrame
      | `Next context -> Frame.updateContext context currentFrame
    in

    let prev = interactionPointerOfFrame frame in

    let frame = match action with
      | `This -> Some frame
      | `Next _ -> Frame.nextInSequence frame
    in

    let rec runToInteraction frame =
      let context = Frame.context frame in
      match frame with

      | _, Frame.SequenceFrame (cur::_rest) ->
        let frame = Frame.make ?prev ~context ~parent:frame cur in
        runToInteraction frame

      | _, Frame.SequenceFrame [] ->
        nextOf frame

      | _, Frame.ProcessFrame (Process.Guard { requires; check; _ }) ->
        if Context.matches context requires
        then (
          let%bind data = fetchFrameData ~config frame in
          let allowed = check context data in
          if allowed
          then nextOf frame
          else bailOf frame
        )
        else bailOf frame

      | _, Frame.ProcessFrame (Process.Query { requires; update; _ }) ->
        if Context.matches context requires
        then (
          let%bind data = fetchFrameData ~config frame in
          let context = update context data in
          let frame = Frame.updateContext context frame in
          nextOf frame
        ) else
          bailOf frame

      | _, Frame.ProcessFrame (Process.Mutation _) ->
        failwith "Mutation is not implemented"

      | _, Frame.ChoiceFrame (cur::_rest) ->
        let frame = Frame.make ?prev ~context ~parent:frame cur in
        runToInteraction frame
      | _, Frame.ChoiceFrame [] ->
        bailOf frame

      | _, Frame.InteractionFrame ({interaction; _} as interactionInfo) ->
        if Context.matches context interaction.requires
        then
          let%bind data = fetchFrameData ~config frame
          and dataTitle = fetchInteractionTitleData ~config ~context interaction
          in
          let interactionInfo = {
            interactionInfo with
            data = Some data;
            dataTitle = Some dataTitle;
          } in
          return (Some interactionInfo, frame)
        else bailOf frame

    and bailOf frame =
      match Frame.nextInChoice frame with
      | Some frame -> runToInteraction frame
      | None -> return (None, frame)

    and nextOf frame =
      match Frame.nextInSequence frame with
      | Some frame -> runToInteraction frame
      | None -> return (None, frame)

    in

    match frame with
    | Some frame ->
      let%bind res, nextFrame = match action with
      | `This ->
        runToInteraction frame
      | `Next context ->
        let frame = Frame.updateContext context currentFrame in
        nextOf frame
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
    let queryTitle context =
      Js.Nullable.to_opt (params##queryTitle context)
    in
    Node.Interaction ({
      Interaction.
      requires = params##requires;
      provides = params##provides;
      query = params##query;
      queryTitle;
      ui = params##ui;
    })

  let guard params =
    let check context data = (params##check context data) in
    Node.Process (Process.Guard {
      Process.
      requires = params##requires;
      query = params##query;
      check;
    })

  let query params =
    let update context data = (params##update context data) in
    Node.Process (Process.Query {
      Process.
      requires = params##requires;
      provides = params##provides;
      query = params##query;
      update = update;
    })

  let sequence nodes =
    let nodes = nodes |> Array.to_list in
    Node.Sequence nodes

  let choice nodes =
    let nodes = nodes |> Array.to_list in
    Node.Choice nodes

  let init = Execution.init

  let resultToJs ~next ~alternatives interaction frame =
    let rec interactionFrameToJs next alternatives (info, frame) =
      let interactionFrameListToJs = Inspect.list (interactionFrameToJs None None) in
      [%bs.obj {
        ui = info.Frame.interaction.ui;
        context = Frame.context frame;
        data = Inspect.option Inspect.id info.data;
        dataTitle = Inspect.option Inspect.id info.dataTitle;
        prev = info.prev |> prev |> List.rev |> interactionFrameListToJs;
        next = Inspect.option interactionFrameListToJs next;
        alternatives = Inspect.option interactionFrameListToJs alternatives;
        frame;
      }]
    and prev = function
      | Some (info, frame) ->
        let rest = prev info.prev in
        (info, frame)::rest
      | None -> []
    in
    match interaction with
    | Some info ->
      let interaction = Js.Nullable.return (
        interactionFrameToJs (Some next) (Some alternatives) (info, frame)
      ) in
      [%bs.obj {frame; interaction}]
    | None ->
      [%bs.obj {frame; interaction = (Js.Nullable.null)}]

  let runToInteraction config currentFrame =
    let open Promise.Syntax in
    let config = { Execution. waitForData = config##waitForData } in
    let%bind interaction, frame = Execution.run ~config ~action:`This currentFrame in
    let%bind next = Execution.next ~config frame in
    let%bind alternatives = Execution.alternatives ~config frame in
    return (resultToJs ~next ~alternatives interaction frame)

  let nextToInteraction config context currentFrame =
    let open Promise.Syntax in
    let config = { Execution. waitForData = config##waitForData } in
    let%bind interaction, frame = Execution.run ~config ~action:(`Next context) currentFrame in
    let%bind next = Execution.next ~config frame in
    let%bind alternatives = Execution.alternatives ~config frame in
    return (resultToJs ~next ~alternatives interaction frame)

end

include JS
