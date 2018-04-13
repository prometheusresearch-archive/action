module Type = Query.Type
module Card = Query.Card
module Typed = Query.Typed
module Scope = Query.Typed.Scope
module Context = Query.Typed.Context
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
  | Type.Value Type.Null ->
    return {
      Type.
      fieldName;
      fieldArgs = StringMap.empty;
      fieldCtyp = Card.Opt, Type.Value Type.Null
    }
  | Type.Value Type.String
  | Type.Value Type.Number
  | Type.Value Type.Bool
  | Type.Value Type.Abstract ->
    let typ = Type.show typ in
    queryTypeError {j|cannot extract field "$fieldName" from value $typ|j}

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

and typeQueryImpl ?(ctx={Typed. ctyp = Type.void; scope = Scope.empty}) ~univ query =


  let rec aux ~ctx ((), syn) =
    let open Run.Syntax in
    let {Typed. scope; _} = ctx in

    let deriveCtx ?(scope=scope) ctx =
      let {Typed. ctyp; _} = ctx in
      {Typed. ctyp; scope}
    in

    let makeCtx ?(scope=scope) ctyp =
      {Typed. ctyp; scope}
    in

    let typedQuery = match syn with

    | Untyped.Void ->
      return (makeCtx Query.Type.void, Typed.Void)

    | Untyped.Here ->

      begin match StringMap.get scope "here" with
      | Some (Typed.UntypedBinding query) ->
        let%bind query = aux ~ctx query in
        let ctx, _ = query in
        return (deriveCtx ctx, Typed.Here query)
      | Some (Typed.TypedBinding query) ->
        let ctx, _ = query in
        return (deriveCtx ctx, Typed.Here query)
      | Some (Typed.HardBinding query) ->
        return query
      | None ->
        return (makeCtx Query.Type.void, Typed.Void)
      end

    | Untyped.Locate (parent, id) ->
      let%bind {ctyp = card, parentTyp; _}, _ as parent = aux ~ctx parent in
      begin match card with
      | Card.Many ->
        let%bind id = aux ~ctx id in
        return Typed.(makeCtx (Card.Opt, parentTyp), Typed.Locate (parent, id))
      | _ ->
        queryTypeError {j|locate can only be applied to queries with cardinality many|j}
      end

    | Untyped.Meta parent ->
      let%bind parent = aux ~ctx parent in
      return Typed.(makeCtx Type.ctyp, Meta parent)

    | Untyped.Grow (parent, next) ->
      let%bind ctx, _ as parent = aux ~ctx parent in
      let%bind ctx, _ as next =
        let ctx =
          let scope = Scope.set scope "here" (Typed.HardBinding parent) in
          deriveCtx ~scope ctx
        in
        aux ~ctx next
      in
      return (deriveCtx ctx, Typed.Grow (parent, next))

    | Untyped.GrowArgs (parent, args) ->
      let%bind ctx, _ as parent = aux ~ctx parent in
      begin match ctx.ctyp with
      | _, Type.Screen { screenName; _ } ->
        let%bind screen = liftResult (Universe.lookupScreenResult screenName univ) in
        let%bind args = checkArgsImpl
          ~updateWithDefaultValues:false
          ~argTyps:screen.args args
        in
        return (deriveCtx ctx, Typed.GrowArgs (parent, args))
      | ctyp ->
        let ctyp = Type.showCt ctyp in
        queryTypeError {j|unable to grow arguments on $ctyp|j}
      end

    | Untyped.Name name ->
      begin match StringMap.get scope name with
      | Some (Typed.UntypedBinding query) ->
        let%bind ctx, _syn as query = aux ~ctx query in
        return (deriveCtx ctx, Typed.Name (name, query))
      | Some (Typed.TypedBinding query)
      | Some (Typed.HardBinding query) ->
        let ctx, _syn = query in
        return (deriveCtx ctx, Typed.Name (name, query))
      | None ->
        queryTypeError {j|referencing unknown binding "$name"|j}
      end

    | Untyped.Define (parent, args) ->
      let%bind ctx, _ as parent =
        let ctx =
          let scope =
            let f scope name value = StringMap.set scope name (Typed.UntypedBinding value)
            in StringMap.reduce args scope f
          in
          deriveCtx ~scope ctx
        in
        aux ~ctx parent
      in
      return (deriveCtx ctx, Typed.Define (parent, args))

    | Untyped.Const (Query.Const.String v) ->
      return Typed.(
        makeCtx (Card.One, Type.Value Type.String),
        Const (Query.Const.String v)
      )
    | Untyped.Const (Query.Const.Number v) ->
      return Typed.(
        makeCtx (Card.One, Type.Value Type.Number),
        Const (Query.Const.Number v)
      )
    | Untyped.Const (Query.Const.Bool v) ->
      return Typed.(
        makeCtx (Card.One, Type.Value Type.Bool),
        Const (Query.Const.Bool v)
      )
    | Untyped.Const (Query.Const.Null) ->
      return Typed.(
        makeCtx (Card.Opt, Type.Value Type.Null),
        Const (Query.Const.Null)
      )

    | Untyped.Count parent ->
      let%bind parent = aux ~ctx parent in
      return Typed.(
        makeCtx (Card.One, Type.Syntax.number),
        Count parent
      )

    | Untyped.First parent ->
      let%bind {ctyp = _, parentTyp;_}, _ as parent = aux ~ctx parent in
      return Typed.(
        makeCtx (Card.Opt, parentTyp),
        First parent
      )

    | Untyped.Screen (parent, { screenName; screenArgs; }) ->
      let%bind parentCtx, _ as parent = aux ~ctx parent in
      let%bind screen = liftResult (Universe.lookupScreenResult screenName univ) in

      let%bind screenArgs = checkArgsImpl
        ~updateWithDefaultValues:true
        ~argTyps:screen.args screenArgs
      in

      let {Typed. ctyp = card, _typ; _} = parentCtx in

      begin match (screen.inputCard, card) with
      | Card.One, Card.Many
      | Card.Opt, Card.Many
      | Card.Many, Card.One
      | Card.Many, Card.Opt ->
        queryTypeError {j|screen "$screenName" cannot be constructed due to cardinality mismatch|j}
      | Card.One, Card.Opt
      | Card.One, Card.One
      | Card.Opt, Card.Opt
      | Card.Opt, Card.One
      | Card.Many, Card.Many ->

        let%bind screenOut =
          Run.context (
            let msg = {j|While expanding screen|j} in
            `QueryTypeError msg
          ) (
            let%bind {Typed. ctyp; _}, _ =
              let parentCtx =
                let scope =
                  let scope =
                    let f scope name q = Scope.set scope name (Typed.UntypedBinding q) in
                    Scope.reduce screenArgs scope f
                  in
                  let scope = Scope.set scope "here" (Typed.HardBinding parent) in
                  let bindings = Scope.set scope "parent" (Typed.TypedBinding parent) in
                  bindings
                in
                deriveCtx ~scope parentCtx
              in
              aux ~ctx:parentCtx screen.grow
            in
            return ctyp
          )
        in

        let ctyp = (
          Card.One,
          Type.Screen { screenName; screenOut; }
        ) in
        let syn = Typed.Screen (parent, { screenName; screenArgs; }) in
        return (makeCtx ctyp, syn)
      end

    | Untyped.Navigate (parent, navigation) ->
      let { Untyped. navName; } = navigation in
      let%bind parent = aux ~ctx parent in
      let navigation = { Typed. navName; } in
      let {Typed. ctyp = parentCard, parentTyp; _}, _parentSyn = parent in
      let%bind field = extractField ~univ navName parentTyp in
      let fieldCard, fieldTyp = field.fieldCtyp in
      let fieldCard = Card.merge parentCard fieldCard in
      return (makeCtx (fieldCard, fieldTyp), Typed.Navigate (parent, navigation))

    | Untyped.Mutation (parent, mut) ->
      let%bind {Typed. ctyp = parentCtyp;_} as parentCtx, _ as parent = aux ~ctx parent in
      let scope = Scope.set scope "here" (Typed.HardBinding parent) in
      let%bind mut = typeMutation ~univ ~ctx:{Typed. ctyp = parentCtyp; scope} mut in
      (** TODO: Need to track mutation in type (as effect probably) *)
      return (deriveCtx parentCtx, Typed.Mutation (parent, mut))

    | Untyped.Select (parent, selection) ->
      let%bind parent = aux ~ctx parent in
      let parentCtx, _parentSyn = parent in
      let {Typed. ctyp = parentCard, _parentTyp; _} = parentCtx in
      let checkField (fields, selection, index) { Untyped. alias; query } =
        let res =
          let%bind query =
            let scope = Scope.set scope "here" (Typed.TypedBinding parent) in
            let ctx = deriveCtx ~scope ctx in
            aux ~ctx query
          in
          let {Typed. ctyp = fieldCard, fieldTyp; _}, _fieldSyn = query in
          let fieldName = Option.getWithDefault (string_of_int index) alias in
          let fieldCard = Card.merge parentCard fieldCard in
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
      return Typed.(makeCtx (parentCard, Type.Record fields), Select (parent, selection))

    | Untyped.LessThan (left, right) ->
      let%bind left = aux ~ctx left in
      let%bind right = aux ~ctx right in
      let syn = Typed.LessThan (left, right) in
      let%bind card =
        match Typed.card left, Typed.card right with
        | Card.One, Card.One -> return Card.One
        | Card.Opt, Card.One
        | Card.One, Card.Opt
        | Card.Opt, Card.Opt -> return Card.Opt
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
        * Handler for Card.Many < Card.One etc
        * *)
      return (makeCtx (card, Type.Value Type.Bool), syn)

    in

    let msg =
      let query = Untyped.show ((), syn) in
      let scope =
        let scope = Scope.mapWithKey scope (fun key _value -> "$" ^ key) in
        scope |> Scope.valuesToArray |> Array.to_list |> String.concat ", "
      in
      let msg = {j|While typing query `$query` with scope: $scope|j} in
      `QueryTypeError msg
    in
    Run.context msg typedQuery

  in

  aux ~ctx query

and typeMutation ~univ ~ctx mut =
  let open Run.Syntax in
  let module Q = Untyped.Syntax in
  let rec typeOp ~ctx map key op =
    let%bind op = match op with
    | Untyped.OpUpdate q ->
      let%bind _ = typeQueryImpl ~univ ~ctx Q.(here |> nav key) in
      let%bind _ = typeQueryImpl ~univ ~ctx q in
      (* TODO: check that types can unified from both nav and value *)
      return (Untyped.OpUpdate q)
    | Untyped.OpUpdateEntity ops ->
      let%bind ctx, _ = typeQueryImpl ~univ ~ctx Q.(here |> nav key) in
      let%bind ops = typeOps ~ctx ops in
      return (Untyped.OpUpdateEntity ops)
    | Untyped.OpCreateEntity ops ->
      let%bind ctx, _ = typeQueryImpl ~univ ~ctx Q.(here |> nav key) in
      let%bind ops = typeOps ~ctx ops in
      return (Untyped.OpCreateEntity ops)
    in
    return (StringMap.set map key op)
  and typeOps ~ctx ops =
    Run.StringMap.foldLeft ~f:(typeOp ~ctx) ~init:StringMap.empty ops
  in
  match mut with
  | Untyped.Update ops ->
    let%bind ops = typeOps ~ctx ops in
    return (Typed.Update ops)
  | Untyped.Create ops ->
    let%bind ops = typeOps ~ctx ops in
    return (Typed.Create ops)

let typeQuery ?ctx ~univ query =
  typeQueryImpl ?ctx ~univ query

let growQuery ~univ ?(scope=Scope.empty) ~base query =
  let open Run.Syntax in
  let ctx =
    let ctx, _ = base in
    let scope =
      let scope = Scope.reduce scope ctx.Typed.scope Scope.set in
      let scope = Scope.set scope "here" (Typed.HardBinding base) in
      scope
    in
    {ctx with scope}
  in
  let%bind {ctyp;_}, _ as query = typeQueryImpl ~ctx ~univ query in
  return ({Typed. ctyp; scope}, Typed.Grow (base, query))

let checkArgs ~argTyps args =
  checkArgsImpl ~updateWithDefaultValues:true ~argTyps args

let checkArgsPartial ~argTyps args =
  checkArgsImpl ~updateWithDefaultValues:false ~argTyps args
