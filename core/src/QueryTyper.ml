module Type = Query.Type
module Typed = Query.Typed
module Scope = Query.Typed.Scope
module Untyped = Query.Untyped
module StringMap = Common.StringMap
module Result = Common.Result
module Option = Common.Option

type error = [ `QueryTypeError of string ]
type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

let queryTypeError err =
  Run.error (`QueryTypeError err)

let liftResult = function
  | Result.Ok v -> Run.return v
  | Result.Error err -> queryTypeError err

let rec extractField ~univ fieldName (typ : Type.t) =
  let open Run.Syntax in
  let findInFieldList fields =
    match Belt.List.getBy fields (fun field -> field.Type.fieldName = fieldName) with
    | None ->
      let typ = Type.show typ in
      queryTypeError {j|no such field "$fieldName" on $typ|j}
    | Some field -> return field
  in
  match typ with
  | Type.Void -> let fields = Universe.fields univ in findInFieldList fields
  | Type.Screen { screenOut = _, typ; _ } ->
    extractField ~univ fieldName typ
  | Type.Entity {entityName = _; entityFields} -> findInFieldList (entityFields typ)
  | Type.Record fields -> findInFieldList fields
  | Type.Value _ ->
    queryTypeError {j|cannot extract field "$fieldName" from value|j}

let navQuery ~univ ~name parent =
  let open Run.Syntax in
  let navigation = { Typed. navName = name; } in
  let (parentCard, parentTyp), _parentSyn = parent in
  let%bind field = extractField ~univ name parentTyp in
  let fieldCard, fieldTyp = field.fieldCtyp in
  let fieldCard = Query.Card.merge parentCard fieldCard in
  let ctx = fieldCard, fieldTyp in
  return (ctx, Typed.Navigate (parent, navigation))

let rec checkArgsImpl
  ~updateWithDefaultValues
  ~argTyps
  args
  =
  let open Run.Syntax in
  (** Type check all passed args first *)
  let%bind args =
    let f args name query =
      let%bind args = args in
      match StringMap.get argTyps name with
      | None -> queryTypeError {j|unknown arg "$name"|j}
      | Some _ ->
        let args = StringMap.set args name query in
        return args
    in StringMap.reduce args (return StringMap.empty) f
  in
  (** Now check if we didn't miss any required arg, add default values *)
  let%bind args =
    if updateWithDefaultValues
    then
      let f args name { Type. argCtyp = _; argDefault } =
        let%bind args = args in
        match StringMap.get args name with
        | None ->
          begin match argDefault with
          | None -> queryTypeError {j|missing required arg "$name"|j}
          | Some query ->
            let args = StringMap.set args name query in
            return args
          end
        | Some _ -> return args
      in StringMap.reduce argTyps (return args) f
    else return args
  in
  return args

and typeQueryImpl ?(ctyp=Type.void) ?(scope=Scope.empty) ~univ query =
  let rec aux ~ctyp ~scope ((), syn) =
    let open Run.Syntax in
    let typedQuery = match syn with

    | Untyped.Void ->
      return (Query.Type.void, Typed.Void)

    | Untyped.Here ->

      begin match StringMap.get scope "here" with
      | Some (Typed.UntypedBinding query) ->
        let%bind query = aux ~ctyp ~scope query in
        let ctyp, _ = query in
        return (ctyp, Typed.Here query)
      | Some (Typed.TypedBinding query) ->
        let ctyp, _ = query in
        return (ctyp, Typed.Here query)
      | Some (Typed.HardBinding query) ->
        return query
      | None -> return (Query.Type.void, Typed.Void)
      end

    | Untyped.Locate (parent, id) ->
      let%bind (card, typ), _ as parent = aux ~ctyp ~scope parent in
      begin match card with
      | Query.Card.Many ->
        let%bind id = aux ~ctyp ~scope id in
        let ctx = Query.Card.Opt, typ in
        return (ctx, Typed.Locate (parent, id))
      | _ ->
        queryTypeError {j|locate can only be applied to queries with cardinality many|j}
      end

    | Untyped.Meta parent ->
      let%bind parent = aux ~ctyp ~scope parent in
      return (Type.ctyp, Typed.Meta parent)

    | Untyped.Grow (parent, next) ->
      let%bind ctyp, _ as parent = aux ~ctyp ~scope parent in
      let scope = Scope.set scope "here" (Typed.HardBinding parent) in
      let%bind ctyp, _ as next = aux ~ctyp ~scope next in
      return (ctyp, Typed.Grow (parent, next))

    | Untyped.GrowArgs (parent, args) ->
      let%bind ctyp, _ as parent = aux ~ctyp ~scope parent in
      begin match ctyp with
      | _, Type.Screen { screenName; _ } ->
        let%bind screen = liftResult (Universe.lookupScreenResult screenName univ) in
        let%bind args = checkArgsImpl
          ~updateWithDefaultValues:false
          ~argTyps:screen.args args
        in
        return (ctyp, Typed.GrowArgs (parent, args))
      | ctyp ->
        let ctyp = Type.showCt ctyp in
        queryTypeError {j|unable to grow arguments on $ctyp|j}
      end

    | Untyped.Name name ->
      begin match StringMap.get scope name with
      | Some (Typed.UntypedBinding query) ->
        let%bind ctyp, _syn as query = aux ~ctyp ~scope query in
        return (ctyp, Typed.Name (name, query))
      | Some (Typed.TypedBinding query)
      | Some (Typed.HardBinding query) ->
        let ctyp, _syn = query in
        return (ctyp, Typed.Name (name, query))
      | None ->
        queryTypeError {j|referencing unknown binding "$name"|j}
      end

    | Untyped.Const (Query.Const.String v) ->
      return (
        (Query.Card.One, Type.Value Type.String),
        Typed.Const (Query.Const.String v)
      )
    | Untyped.Const (Query.Const.Number v) ->
      return (
        (Query.Card.One, Type.Value Type.Number),
        Typed.Const (Query.Const.Number v)
      )
    | Untyped.Const (Query.Const.Bool v) ->
      return (
        (Query.Card.One, Type.Value Type.Bool),
        Typed.Const (Query.Const.Bool v)
      )
    | Untyped.Const (Query.Const.Null) ->
      return (
        (Query.Card.Opt, Type.Value Type.Null),
        Typed.Const (Query.Const.Null)
      )

    | Untyped.Count parent ->
      let%bind parent = aux ~ctyp ~scope parent in
      return (
        (Query.Card.One, Type.Syntax.number),
        Typed.Count parent
      )

    | Untyped.First parent ->
      let%bind (_, parentTyp), _ as parent = aux ~ctyp ~scope parent in
      return (
        (Query.Card.Opt, parentTyp),
        Typed.First parent
      )

    | Untyped.Screen (parent, { screenName; screenArgs; }) ->
      let%bind parentCtyp, _ as parent = aux ~ctyp ~scope parent in
      let%bind screen = liftResult (Universe.lookupScreenResult screenName univ) in

      let%bind screenArgs = checkArgsImpl
        ~updateWithDefaultValues:true
        ~argTyps:screen.args screenArgs
      in

      let (card, _typ) = parentCtyp in

      begin match (screen.inputCard, card) with
      | Query.Card.One, Query.Card.Many
      | Query.Card.Opt, Query.Card.Many
      | Query.Card.Many, Query.Card.One
      | Query.Card.Many, Query.Card.Opt ->
        queryTypeError {j|screen "$screenName" cannot be constructed due to cardinality mismatch|j}
      | Query.Card.One, Query.Card.Opt
      | Query.Card.One, Query.Card.One
      | Query.Card.Opt, Query.Card.Opt
      | Query.Card.Opt, Query.Card.One
      | Query.Card.Many, Query.Card.Many ->

        let%bind screenOut, _ =
          let scope =
            let scope =
              let f scope name q = Scope.set scope name (Typed.UntypedBinding q) in
              Scope.reduce screenArgs scope f
            in
            let scope = Scope.set scope "here" (Typed.HardBinding parent) in
            let bindings = Scope.set scope "parent" (Typed.TypedBinding parent) in
            bindings
          in
          Run.context (
            let msg = {j|While expanding screen|j} in
            `QueryTypeError msg
          ) (
            aux ~ctyp:parentCtyp ~scope screen.grow
          )
        in

        let ctyp = (
          Query.Card.One,
          Type.Screen { screenName; screenOut; }
        ) in
        let syn = Typed.Screen (parent, { screenName; screenArgs; }) in
        return (ctyp, syn)
      end

    | Untyped.Navigate (parent, navigation) ->
      let { Untyped. navName; } = navigation in
      let%bind parent = aux ~ctyp ~scope parent in
      navQuery ~univ ~name:navName parent

    | Untyped.Mutation (parent, mut) ->
      let%bind parentCtyp, _ as parent = aux ~ctyp ~scope parent in
      let scope = Scope.set scope "here" (Typed.HardBinding parent) in
      let%bind mut = typeMutation ~univ ~ctyp:parentCtyp ~scope mut in
      (** TODO: Need to track mutation in type (as effect probably) *)
      return (parentCtyp, Typed.Mutation (parent, mut))

    | Untyped.Select (parent, selection) ->
      let%bind parent = aux ~ctyp ~scope parent in
      let parentCtx, _parentSyn = parent in
      let (parentCard, _parentTyp) = parentCtx in
      let checkField (fields, selection, index) { Untyped. alias; query } =
        let res =
          let scope = Scope.set scope "here" (Typed.TypedBinding parent) in
          let%bind query = aux ~ctyp ~scope query in
          let (fieldCard, fieldTyp), _fieldSyn = query in
          let fieldName = Option.getWithDefault (string_of_int index) alias in
          let fieldCard = Query.Card.merge parentCard fieldCard in
          let fieldCtyp = fieldCard, fieldTyp in
          let field = { Type. fieldCtyp; fieldName; fieldArgs = StringMap.empty } in
          let selectionField = { Typed. alias; query; } in
          return (field::fields, selectionField::selection, index + 1)
        in
        Run.context (`QueryTypeError {j|While typing field `$alias`|j}) res
      in
      let%bind (fields, selection, _) =
        let init = ([], [], 0) in
        Run.List.foldLeft ~f:checkField ~init selection
      in
      let typ = Type.Record fields in
      let ctyp = parentCard, typ in
      return (ctyp, Typed.Select (parent, selection))

    | Untyped.LessThan (left, right) ->
      let%bind left = aux ~ctyp ~scope left in
      let%bind right = aux ~ctyp ~scope right in
      let syn = Typed.LessThan (left, right) in
      let%bind card =
        match Typed.card left, Typed.card right with
        | Query.Card.One, Query.Card.One -> return Query.Card.One
        | Query.Card.Opt, Query.Card.One
        | Query.Card.One, Query.Card.Opt
        | Query.Card.Opt, Query.Card.Opt -> return Query.Card.Opt
        | _ ->
          queryTypeError "'<' cardinality mismatch: expected one / opt"
      in
      let%bind () =
        match Typed.typ left, Typed.typ right with
        | Type.Value Type.Number, Type.Value Type.Number
        | Type.Value Type.Number, Type.Value Type.Null
        | Type.Value Type.Null, Type.Value Type.Number
        | Type.Value Type.Null, Type.Value Type.Null ->
          return ()
        | _ ->
          queryTypeError "'<' type mismatch: numbers expected"
      in
      (* TODO:
        * Handler for Query.Card.Many < Query.Card.One etc
        * *)
      return ((card, Type.Value Type.Bool), syn)

    in

    let msg =
      let query = Untyped.show ((), syn) in
      let msg = {j|While typing query `$query`|j} in
      `QueryTypeError msg
    in
    Run.context msg typedQuery

  in

  aux ~ctyp ~scope query

and typeMutation ~univ ~ctyp ~scope mut =
  let open Run.Syntax in
  let module Q = Untyped.Syntax in
  let rec typeOp ~ctyp map key op =
    let%bind op = match op with
    | Untyped.OpUpdate q ->
      let%bind _ = typeQueryImpl ~univ ~ctyp ~scope Q.(here |> nav key) in
      let%bind _ = typeQueryImpl ~univ ~ctyp ~scope q in
      (* TODO: check that types can unified from both nav and value *)
      return (Untyped.OpUpdate q)
    | Untyped.OpUpdateEntity ops ->
      let%bind ctyp, _ = typeQueryImpl ~univ ~ctyp ~scope Q.(here |> nav key) in
      let%bind ops = typeOps ~ctyp ops in
      return (Untyped.OpUpdateEntity ops)
    | Untyped.OpCreateEntity ops ->
      let%bind ctyp, _ = typeQueryImpl ~univ ~ctyp ~scope Q.(here |> nav key) in
      let%bind ops = typeOps ~ctyp ops in
      return (Untyped.OpCreateEntity ops)
    in
    return (StringMap.set map key op)
  and typeOps ~ctyp ops =
    Run.StringMap.foldLeft ~f:(typeOp ~ctyp) ~init:StringMap.empty ops
  in
  match mut with
  | Untyped.Update ops ->
    let%bind ops = typeOps ~ctyp ops in
    return (Typed.Update ops)
  | Untyped.Create ops ->
    let%bind ops = typeOps ~ctyp ops in
    return (Typed.Create ops)

let typeQuery ?ctyp ?(scope=Scope.empty) ~univ query =
  typeQueryImpl ?ctyp ~scope ~univ query

let growQuery ~univ ?(scope=Scope.empty) ~base query =
  let open Run.Syntax in
  let ctyp, _ = base in
  let scope = Scope.set scope "here" (Typed.HardBinding base) in
  let%bind ctyp, _ as query = typeQueryImpl ~ctyp ~scope ~univ query in
  return (ctyp, Typed.Grow (base, query))

let checkArgs ~argTyps args =
  checkArgsImpl ~updateWithDefaultValues:true ~argTyps args

let checkArgsPartial ~argTyps args =
  checkArgsImpl ~updateWithDefaultValues:false ~argTyps args
