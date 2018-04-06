module Type = Query.Type

type t = {
  fields : Type.field list;
  screens : Screen.t Common.StringMap.t;
}

let empty = { fields = []; screens = Common.StringMap.empty }

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
  { univ with screens = Common.StringMap.set univ.screens name screen; }

let fields univ = univ.fields

let lookupScreen name univ =
  Common.StringMap.get univ.screens name

let lookupScreenResult name univ =
  Common.Result.ofOption
    ~err:{j|no such screen "$name"|j}
    (lookupScreen name univ)
