module Result = Common.Result
module Option = Common.Option
module Card = Query.Card
module Type = Query.Type
module Const = Query.Const
module Typed = Query.Typed
module StringMap = Common.StringMap

type t = {
  value : Value.t;
  univ : Universe.t;
}

type error = [ `DatabaseError of string | QueryTyper.error ]
type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

let liftResult = function
  | Result.Ok v -> Run.return v
  | Result.Error err -> Run.error (`DatabaseError err)

let liftOption ~err = function
  | Some v -> Run.return v
  | None -> Run.error (`DatabaseError err)

let executionError err = Run.error (`DatabaseError err)

module Ref = struct
  type t = {
    name : string;
    id : string;
  }

  let ofValue value =
    let open Common.Option.Syntax in
    match Value.classify value with
    | Value.Object dict ->
      let%bind ref = Js.Dict.get dict "$ref" in
      let%bind ref = Value.decodeObj ref in
      let%bind name = Js.Dict.get ref "entity" in
      let%bind name = Value.decodeString name in
      let%bind id = Js.Dict.get ref "id" in
      let%bind id = Value.decodeString id in
      return {name; id}
    | _ -> None

  let toValue ref =
    let ref =
      let v = Js.Dict.empty () in
      Js.Dict.set v "entity" (Value.string ref.name);
      Js.Dict.set v "id" (Value.string ref.id);
      (Value.obj v)
    in
    let v = Js.Dict.empty () in
    Js.Dict.set v "$ref" ref;
    (Value.obj v)

end

module Cache = struct
  include Common.MutStringMap
end

let univ {univ;_} = univ

let root {value;_} = value

let ofJson ~univ value =
  let value = value |> Value.ofJson in
  {univ; value}

let ofStringExn ~univ value =
  let value = value |> Js.Json.parseExn |> Value.ofJson in
  {univ; value}

external jsDictAssign : 'a Js.Dict.t -> 'a Js.Dict.t -> unit =
  "assign" [@@bs.val] [@@bs.scope "Object"]

let getEntityInternal ~db ~name ~id =
    let resolved =
      let open! Option.Syntax in
      let%bind coll = Value.get ~name:name db.value in
      let%bind value = Value.get ~name:id coll in
      return value
    in
    begin match resolved with
    | Some value -> Run.return value
    | None -> executionError {j|unable to resolve ref $name@$id|j}
    end

let setEntityInternal ~db ~name ~id value =
    let open Run.Syntax in
    let%bind coll = liftOption ~err:"unknown collection" (Value.get ~name:name db.value) in
    let%bind coll = liftOption ~err:"malformed collection" (Value.decodeObj coll) in
    Js.Dict.set coll id value;
    return ()

let updateEntityInternal ~db ~name ~id update =
    let open Run.Syntax in
    let%bind value = getEntityInternal ~db ~name ~id in
    let%bind value = liftOption ~err:"invalid entity" (Value.decodeObj value) in
    let%bind update = liftOption ~err:"invalid entity update" (Value.decodeObj update) in
    jsDictAssign value update;
    return ()

let generateEntityId ~db ~name =
    let open Run.Syntax in
    let%bind coll = liftOption ~err:"unknown collection" (Value.get ~name:name db.value) in
    let%bind coll = liftOption ~err:"malformed collection" (Value.decodeObj coll) in
    let length = Array.length (Js.Dict.keys coll) in
    return {j|$name.$length|j}

(*
 * Format value by removing all not explicitly mentioned references according to
 * the type info and query result.
 *)
let formatValue ~ctyp value =

  let open Run.Syntax in

  let rec filterOutRefsAndRecurse ~fields input =
    let%bind obj =
      let f output {Type. fieldName; fieldCtyp} =
        match fieldCtyp with
        | _, Type.Entity _ -> return output
        | _, _ ->
          begin match Js.Dict.get input fieldName with
          | Some value ->
            let%bind value = format ~ctyp:fieldCtyp value in
            Js.Dict.set output fieldName value;
            return output
          | None -> return output
          end
      in
      Run.List.foldLeft ~f ~init:(Js.Dict.empty ()) fields
    in
    return (Value.obj obj)

  and recurse ~fields input =
    let%bind obj =
      let f output {Type. fieldName; fieldCtyp} =
        match Js.Dict.get input fieldName with
        | Some value ->
          let%bind value = format ~ctyp:fieldCtyp value in
          Js.Dict.set output fieldName value;
          return output
        | None -> return output
      in
      Run.List.foldLeft ~f ~init:(Js.Dict.empty ()) fields
    in
    return (Value.obj obj)

  and recurseIntoArrayWith ~recurse ~fields items =
    let f value =
      match Value.classify value with
      | Value.Object obj -> recurse ~fields obj
      | _ -> return value
    in
    let%bind items = Run.Array.map ~f items in
    return (Value.array items)

  and format ~ctyp value =
    match ctyp, Value.classify value with
    | (Card.One, (Type.Entity {entityFields; _} as typ)), Value.Object value
    | (Card.Opt, (Type.Entity {entityFields; _} as typ)), Value.Object value ->
      filterOutRefsAndRecurse ~fields:(entityFields typ) value
    | (Card.Many, (Type.Entity {entityFields; _} as typ)), Value.Array items ->
      recurseIntoArrayWith ~recurse:filterOutRefsAndRecurse ~fields:(entityFields typ) items

    | (Card.One, Type.Record fields), Value.Object obj
    | (Card.Opt, Type.Record fields), Value.Object obj ->
      recurse ~fields obj
    | (Card.Many, Type.Record fields), Value.Array items ->
      recurseIntoArrayWith ~recurse ~fields items
    | _ -> return value
  in

  format ~ctyp value

let query ?value ~db q =
  let open Run.Syntax in

  let isRoot value = value == db.value in

  let rec navigate name value =
    match Value.classify value with
    | Value.Object obj ->
      begin match Js.Dict.get obj name with
      | Some value ->
        let%bind value = maybeExpandRef value in
        return value
      | None ->
        let msg = {j|no such key "$name"|j} in
        (* Js.log3 "ERROR:" msg [%bs.obj { data = value; key = name; }]; *)
        executionError msg
      end
    | Value.Null -> return Value.null
    | _ -> executionError "cannot traverse this"

  and navigateFromRoot name value =
    let%bind value = navigate name value in
    match Value.classify value with
    | Value.Object value ->
      let value = Js.Dict.values value in
      return (Value.array value)
    | _ -> executionError "invalid db structure: expected an entity collection"

  and aux ~(value : Value.t) ~cache ({Query.Typed. ctyp = _card, typ; scope}, syn as q) =

    (** Precompute all typed bindings values as typed binding values has
     * call-by-value semantics *)
    let%bind () =
      let f (uniqName, name, binding) =
        let uniqName = Scope.Name.toString uniqName in
        match name, binding, Cache.has cache uniqName with
        | "here", _, _ ->
          return ()
        | _, Query.Typed.TypedBinding q, false ->
          let%bind value = aux ~cache ~value q in
          Cache.set cache uniqName value;
          return ()
        | _, Query.Typed.TypedBinding _, true
        | _, Query.Typed.UntypedBinding _, _ ->
          return ()
      in
      Run.List.iter ~f (Scope.bindings scope);
    in

    let value = match typ, syn with
    | Type.Void, Query.Typed.Void ->
      return db.value

    | _, Query.Typed.Void ->
      executionError "invalid type for void"

    | _, Query.Typed.Here ->
      return value

    | _, Query.Typed.Name (name, query) ->
      (** TypedBinding values should be pre-cached already *)
      begin match Scope.get name scope, Cache.get cache (Scope.Name.toString name) with
      | Some (Typed.TypedBinding _), Some value -> return value
      | _ -> aux ~value ~cache query
      end

    | _, Query.Typed.Define (parent, _args) ->
      aux ~value ~cache parent

    | Type.Value Type.String, Query.Typed.Const (Const.String v) ->
      return (Value.string v)
    | Type.Value Type.Number, Query.Typed.Const (Const.Number v) ->
      return (Value.number v)
    | Type.Value Type.Bool, Query.Typed.Const (Const.Bool v) ->
      return (Value.bool v)
    | Type.Value Type.Null, Query.Typed.Const (Const.Null) ->
      return (Value.null)
    | _, Query.Typed.Const _ ->
      executionError "invalid type for const"

    | Type.Value Type.Number, Query.Typed.Count query ->
      let%bind value = aux ~value ~cache query in
      begin match Value.classify value with
      | Value.Null -> return (Value.number 0.)
      | Value.Array items -> return (Value.number (float_of_int (Array.length items)))
      | _ -> return (Value.number 1.)
      end
    | _, Query.Typed.Count _ ->
      executionError "invalid type for count"

    | _, Query.Typed.First query ->
      let%bind value = aux ~value ~cache query in
      begin match Value.classify value with
      | Value.Array items ->
        if Array.length items > 0
        then return (Array.get items 0)
        else return Value.null
      | _ -> return value
      end

    | Type.Screen _ as typ, Query.Typed.Screen (query, { screenName; screenArgs; }) ->

      let make value =
        match Value.classify value with
        | Value.Null -> return Value.null
        | _ ->
          let univ = univ db in
          let%bind screen = liftResult (Universe.lookupScreenResult screenName univ) in
          let ui =
            Value.UI.make
              ~univ
              ~screen
              ~name:screenName
              ~args:screenArgs
              ~typ
              ~value
              ~parentQuery:query
          in
          return (Value.ui ui)
      in

      (* XXX: ideally we should prefetch just exists(query) instead of query
       * itself so we can minimize the work for db to do.
       *)
      let%bind prefetch = aux ~value ~cache query in
      begin match Value.classify prefetch with
      | Value.Null -> return Value.null
      | _ -> make value
      end
    | _, Query.Typed.Screen _ ->
      executionError "invalid type for screen"

    | _, Query.Typed.Navigate (query, { navName; }) ->
      let%bind value = aux ~value ~cache query in
      let%bind value = maybeExpandRef value in

      begin match isRoot value, Value.classify value with

      | true, Value.Object _ ->
        navigateFromRoot navName value
      | true, _ ->
        executionError "invalid db structure: expected an object as the root"

      | _, Value.Object _ -> navigate navName value
      | _, Value.Array items  ->
        let%bind items = Run.Array.map ~f:(navigate navName) items in
        let items =
          let f res item =
            match Value.classify item with
            | Value.Array item -> Belt.Array.concat res item
            | _ -> ignore (Js.Array.push item res); res
          in
          Belt.Array.reduce items (Belt.Array.makeUninitializedUnsafe 0) f in
        return (Value.array items)
      | _, Value.Null ->
        return Value.null
      | _, Value.UI ui ->
        let%bind outQuery = Value.UI.outQuery ui in
        let queryValue = Value.UI.value ui in
        let%bind value = aux ~value:queryValue ~cache outQuery in
        navigate navName value
      | _ -> executionError {|Cannot navigate away from this value|}
      end

    | Type.Record _, Query.Typed.Select (query, selection) ->
      let selectFrom value =
        let%bind _, dataset =
          let build (idx, dataset) { Query.Typed. alias; query; } =
            let%bind selectionValue = aux ~value ~cache query in
            let selectionAlias = Option.getWithDefault (string_of_int idx) alias in
            Js.Dict.set dataset selectionAlias selectionValue;
            return (idx + 1, dataset)
          in
          Run.List.foldLeft ~f:build ~init:(0, Js.Dict.empty ()) selection
        in
        return (Value.obj dataset)
      in
      let%bind value = aux ~value ~cache query in
      begin match Value.classify value with
      | Value.Object _ -> selectFrom value
      | Value.UI ui ->
        let%bind outQuery = Value.UI.outQuery ui in
        let queryValue = Value.UI.value ui in
        let%bind value = aux ~value:queryValue ~cache outQuery in
        selectFrom value
      | Value.Array items ->
        let%bind items = Run.Array.map ~f:selectFrom items in
        return (Value.array items)
      | Value.Null ->
        return Value.null
      | _ ->
        Js.log3 "ERROR" "cannot select from this value" value;
        executionError "cannot select from here"
      end
    | _, Query.Typed.Select _ ->
      executionError "invalid type for select"

    | _, Query.Typed.Locate (parent, id) ->
      let%bind parent = aux ~value ~cache parent in
      begin match Value.classify parent with
      | Value.Null ->
        return Value.null
      | Value.Array items ->
        let%bind id = aux ~value ~cache id in
        let f v =
          match Value.get ~name:"id" v with
          | Some v -> v = id
          | None -> false
        in
        return (Value.ofOption (Js.Array.find f items))
      | _ ->
        Js.log3 "ERROR:" "expected array but got" (Js.typeof parent);
        executionError {j|expected array|j}
      end

    | _, Query.Typed.Meta ({ctyp;_}, _) ->
      return (Value.ofCtyp ctyp)

    | _, Query.Typed.Grow (parent, next) ->
      let%bind value = aux ~value ~cache parent in
      let%bind value = aux ~value ~cache next in
      return value

    | _, Query.Typed.GrowArgs (parent, args) ->
      let%bind value = aux ~value ~cache parent in
      begin match Value.classify value with
      | Value.UI ui ->
        let ui = Value.UI.setArgs ~args ui in
        return (Value.ui ui)
      | _ ->
        executionError "unable to grow args"
      end

    | _, Query.Typed.Mutation (parent, Query.Typed.Update ops) ->
      let%bind _ = updateEntity ~value ~query:parent ~cache ops in
      aux ~value ~cache parent

    | _, Query.Typed.Mutation (parent, Query.Typed.Create ops) ->
      let%bind _ = createEntity ~value ~query:parent ~cache ops in
      aux ~value ~cache parent

    | _, Query.Typed.LessThan (left, right) ->
      let%bind left = aux ~value ~cache left in
      let%bind right = aux ~value ~cache right in
      begin
        match Value.classify left, Value.classify right with
        | Value.Number left, Value.Number right ->
          return (Value.bool (left < right))
        | Value.Null, Value.Number _
        | Value.Number _, Value.Null
        | Value.Null, Value.Null ->
          return Value.null
        | _ ->
          executionError "'<' type mismatch ..."
      end

    in

    Run.context (
      let query = Query.Typed.show q in
      let msg = {j|While evaluating $query|j} in
      `DatabaseError msg
    ) value

  and maybeExpandRef value =
    let resolveRef value =
      match Ref.ofValue value with
      | Some ref ->
        getEntityInternal ~db ~name:ref.name ~id:ref.id
      | None -> return value
    in
    match Value.classify value with
    | Value.Object _ -> resolveRef value
    | Value.Array items ->
      let%bind items = Run.Array.map ~f:resolveRef items in
      return (Value.array items)
    | _ -> return value

  and createEntity ~query ~value ~cache mut =
    let {Query.Typed. ctyp;_}, _ = query in
    match ctyp with
    | _, Query.Type.Entity {entityName;_} ->
      let dict = Js.Dict.empty () in
      let%bind () = mut |> StringMap.toList |> Run.List.iter ~f:(applyOp ~value ~query ~cache dict) in
      let value = (Value.obj dict) in
      let%bind id = generateEntityId ~db ~name:entityName in
      let%bind () = setEntityInternal ~db ~name:entityName ~id value in
      return id
    | ctyp ->
      let query = Query.Typed.show query in
      let ctyp = Query.Type.showCt ctyp in
      executionError {j|createEntity could not be called at $query of type $ctyp|j}

  and updateEntity ~query ~value ~cache ops =
    let {Query.Typed. ctyp;_}, _ = query in
    match ctyp with
    | Card.One, Query.Type.Entity {entityName;_}
    | Card.Opt, Query.Type.Entity {entityName;_} ->
      let%bind entity = aux ~value ~cache query in
      let%bind dict = liftOption ~err:"invalid entity structure" (Value.decodeObj entity) in
      let%bind () = ops |> StringMap.toList |> Run.List.iter ~f:(applyOp ~value:entity ~query ~cache dict) in
      let%bind id = liftOption ~err:"No ID after update" (Js.Dict.get dict "id") in
      let%bind id = liftOption ~err:"Invalid ID" (Value.decodeString id) in
      let%bind () = updateEntityInternal ~db ~name:entityName ~id (Value.obj dict) in
      return id
    | ctyp ->
      let query = Query.Typed.show query in
      let ctyp = Query.Type.showCt ctyp in
      executionError {j|updateEntity could not be called at $query of type $ctyp|j}

  and applyOp ~value ~query ~cache dict =
    function
    | key, Query.Untyped.OpUpdateEntity ops ->
      let%bind query =
        QueryTyper.growQuery
          ~univ:(univ db)
          ~base:query
          Query.Untyped.Syntax.(here |> nav key)
      in
      let%bind _ = updateEntity ~value ~query ~cache ops in
      return ()
    | key, Query.Untyped.OpCreateEntity ops ->
      let%bind query =
        QueryTyper.growQuery
          ~univ:(univ db)
          ~base:query
          Query.Untyped.Syntax.(here |> nav key)
      in
      let%bind id = createEntity ~value ~query ~cache ops in
      Js.Dict.set dict key (Ref.toValue {Ref. name = key; id = id});
      return ()
    | key, Query.Untyped.OpUpdate q ->
      let univ = univ db in
      let%bind q = QueryTyper.growQuery ~univ ~base:query q in
      let%bind value = aux ~value ~cache q in
      Js.Dict.set dict key value;
      return ()

  in

  let value = match value with
  | Some value -> value
  | None -> db.value
  in

  let cache = Cache.make () in
  let%bind value = aux ~value ~cache q in
  let%bind value = maybeExpandRef value in

  let%bind value =
    let {Query.Typed. ctyp;_}, _ = q in
    formatValue ~ctyp value
  in

  return value
