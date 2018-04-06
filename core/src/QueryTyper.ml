module Type = Query.Type
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

let nav ~univ name parent =
  let open Run.Syntax in
  let navigation = { Query.Typed. navName = name; } in
  let (scope, (parentCard, parentTyp)), _parentSyn = parent in
  let%bind field = extractField ~univ name parentTyp in
  let fieldCard, fieldTyp = field.fieldCtyp in
  let fieldCard = Query.Card.merge parentCard fieldCard in
  return ((scope, (fieldCard, fieldTyp)), Query.Typed.Navigate (parent, navigation))

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

and typeQueryImpl ?here ?(ctx=Query.Typed.Context.void) ~univ query =
  let rec aux ~here ~ctx ((), query) =
    let scope, ctyp = ctx in
    let open Run.Syntax in
    match query with
    | Query.Untyped.Void ->
      return ((scope, (Query.Card.One, Type.Void)), Query.Typed.Void)
    | Query.Untyped.Here ->
      begin match here with
      | Some here -> return here
      | None -> return (ctx, Query.Typed.Here)
      end
    | Query.Untyped.Locate (parent, id) ->
      let%bind ((bindings, (card, typ)), _) as parent = aux ~here ~ctx parent in
      begin match card with
      | Query.Card.Many ->
        let%bind id = aux ~here ~ctx id in
        return ((bindings, (Query.Card.Opt, typ)), Query.Typed.Locate (parent, id))
      | _ ->
        queryTypeError {j|locate can only be applied to queries with cardinality many|j}
      end

    | Query.Untyped.Meta parent ->
      let%bind (scope, _ctyp), _ as parent = aux ~here ~ctx parent in
      return ((scope, Type.ctyp), Query.Typed.Meta parent)

    | Query.Untyped.Grow (parent, next) ->
      let%bind ctx, _ as parent = aux ~here ~ctx parent in
      let%bind ctx, _ as next = aux ~here ~ctx next in
      return (ctx, Query.Typed.Grow (parent, next))

    | Query.Untyped.Name name ->
      begin match StringMap.get scope name with
      | Some (Query.Typed.Binding query) ->
        let%bind nextCtx, _syn as query = aux ~here ~ctx query in
        return (nextCtx, Query.Typed.Name (name, query))
      | Some (Query.Typed.TypedBinding query) ->
        let nextCtx, _syn = query in
        let nextCtx =
          Query.Typed.Context.addBindings
            ~bindings:(Query.Typed.Context.bindings ctx)
            nextCtx
        in
        return (nextCtx, Query.Typed.Name (name, query))
      | None ->
        queryTypeError {j|referencing unknown binding "$name"|j}
      end

    | Query.Untyped.Where (parent, bindings) ->
      let nextScope =
        let f scope name query =
          StringMap.set scope name (Query.Typed.Binding query)
        in
        StringMap.reduce bindings scope f
      in
      let%bind ((_, parentCtyp), _) as parent = aux ~here ~ctx:(nextScope, ctyp) parent in
      return (
        (scope, parentCtyp),
        Query.Typed.Where (parent, bindings)
      )
    | Query.Untyped.Const (Query.Const.String v) ->
      return (
        (scope, (Query.Card.One, Type.Value Type.String)),
        Query.Typed.Const (Query.Const.String v)
      )
    | Query.Untyped.Const (Query.Const.Number v) ->
      return (
        (scope, (Query.Card.One, Type.Value Type.Number)),
        Query.Typed.Const (Query.Const.Number v)
      )
    | Query.Untyped.Const (Query.Const.Bool v) ->
      return (
        (scope, (Query.Card.One, Type.Value Type.Bool)),
        Query.Typed.Const (Query.Const.Bool v)
      )
    | Query.Untyped.Const (Query.Const.Null) ->
      return (
        (scope, (Query.Card.Opt, Type.Value Type.Null)),
        Query.Typed.Const (Query.Const.Null)
      )
    | Query.Untyped.Count parent ->
      let%bind parent = aux ~here ~ctx parent in
      return (
        (scope, (Query.Card.One, Type.Syntax.number)),
        Query.Typed.Count parent
      )
    | Query.Untyped.First parent ->
      let%bind ((_, (_, parentType)), _) as parent = aux ~here ~ctx parent in
      return (
        (scope, (Query.Card.Opt, parentType)),
        Query.Typed.First parent
      )
    | Query.Untyped.Screen (parent, { screenName; screenArgs; }) ->
      let%bind (parentCtx, _) as parent = aux ~here ~ctx parent in
      let%bind screen = liftResult (Universe.lookupScreenResult screenName univ) in

      let%bind screenArgs = checkArgsImpl
        ~updateWithDefaultValues:true
        ~argTyps:screen.args screenArgs
      in

      let parentScope, (card, _typ) = parentCtx in
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

        let%bind (_, screenOut), _ =
          let bindings =
            let bindings = StringMap.map screenArgs (fun a -> Query.Typed.Binding a) in
            let bindings = match StringMap.get parentScope "parent" with
            | Some _ -> bindings
            | None -> StringMap.set bindings "parent" (TypedBinding parent)
            in
            bindings
          in
          let ctx = Query.Typed.Context.addBindings ~bindings parentCtx in
          aux ~here:(Some parent) ~ctx screen.grow
        in

        let ctx = (
          parentScope,
          (
            Query.Card.One,
            Type.Screen { screenName; screenOut; }
          )
        ) in
        let syn = Query.Typed.Screen (parent, { screenName; screenArgs; }) in
        return (ctx, syn)
      end

    | Query.Untyped.Navigate (parent, navigation) ->
      let { Query.Untyped. navName; } = navigation in
      let%bind parent = aux ~here ~ctx parent in
      nav ~univ navName parent

    | Query.Untyped.Select (parent, selection) ->
      let%bind parent = aux ~here ~ctx parent in
      let parentCtx, _parentSyn = parent in
      let parentScope, (parentCard, _parentTyp) = parentCtx in
      let checkField (fields, selection, index) { Query.Untyped. alias; query } =
        let%bind query = aux ~here:None ~ctx:parentCtx query in
        let (_fieldScope, (fieldCard, fieldTyp)), _fieldSyn = query in
        let fieldName = Option.getWithDefault (string_of_int index) alias in
        let fieldCard = Query.Card.merge parentCard fieldCard in
        let fieldCtyp = fieldCard, fieldTyp in
        let field = { Type. fieldCtyp; fieldName; fieldArgs = StringMap.empty } in
        let selectionField = { Query.Typed. alias; query; } in
        return (field::fields, selectionField::selection, index + 1)
      in
      let%bind (fields, selection, _) =
        let init = ([], [], 0) in
        Run.List.foldLeft ~f:checkField ~init selection
      in
      let typ = Type.Record fields in
      return ((parentScope, (parentCard, typ)), Query.Typed.Select (parent, selection))

    | Query.Untyped.LessThan (left, right) ->
      let%bind left = aux ~here ~ctx left in
      let%bind right = aux ~here ~ctx right in
      let syn = Query.Typed.LessThan (left, right) in
      let%bind card =
        match Query.Typed.card left, Query.Typed.card right with
        | Query.Card.One, Query.Card.One -> return Query.Card.One
        | Query.Card.Opt, Query.Card.One
        | Query.Card.One, Query.Card.Opt
        | Query.Card.Opt, Query.Card.Opt -> return Query.Card.Opt
        | _ ->
          queryTypeError "'<' cardinality mismatch: expected one / opt"
      in
      let%bind () =
        match Query.Typed.typ left, Query.Typed.typ right with
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
      return ((scope, (card, Type.Value Type.Bool)), syn)


  in aux ~here ~ctx query

let typeQuery ?ctx ~univ query =
  typeQueryImpl ?ctx ~univ query

let growQuery ?bindings ~univ ~base query =
  let ctx, _ = base in
  let ctx = match bindings with
  | Some bindings -> Query.Typed.Context.addBindings ~bindings ctx
  | None -> ctx
  in
  typeQueryImpl ~here:base ~ctx ~univ query

let checkArgs ~argTyps args =
  checkArgsImpl ~updateWithDefaultValues:true ~argTyps args

let checkArgsPartial ~argTyps args =
  checkArgsImpl ~updateWithDefaultValues:false ~argTyps args
