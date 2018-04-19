module Type = Query.Type
module Result = Common.Result
module Map = Common.StringMap

type t = {
  fields : Type.field list;
  entities : Type.entity Map.t;
  screens : Screen.t Map.t;
}

let empty = {
  fields = [];
  entities = Map.empty;
  screens = Map.empty;
}

let hasOne ?args name typ univ =
  let field = Type.Syntax.hasOne ?args name typ in
  { univ with fields = field::univ.fields }

let hasOpt ?args name typ univ =
  let field = Type.Syntax.hasOpt ?args name typ in
  { univ with fields = field::univ.fields }

let hasMany ?args name typ univ =
  let field = Type.Syntax.hasMany ?args name typ in
  { univ with fields = field::univ.fields }

let hasScreen name screen univ =
  { univ with screens = Map.set univ.screens name screen; }

let fields univ = univ.fields

let lookupEntity name univ =
  Map.get univ.entities name

let lookupScreen name univ =
  Map.get univ.screens name

let lookupScreenResult name univ =
  Result.ofOption
    ~err:{j|no such screen "$name"|j}
    (lookupScreen name univ)

let lookupEntityResult name univ =
  Result.ofOption
    ~err:{j|no such entity "$name"|j}
    (lookupEntity name univ)
