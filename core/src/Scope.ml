module StringMap = Common.StringMap

type name = string

module Name = struct
  type t = string
  let toString v = v
end

type 'v t = {
  values : (name * 'v) StringMap.t;
  stack : string StringMap.t list;
}

let empty = {values = StringMap.empty; stack = []}

let get name {values; _} =
  let open Common.Option.Syntax in
  let%bind _, v = StringMap.get values name in
  return v

let resolve name {stack; _} =
  let rec aux =
    function
    | curr::rest ->
      begin match StringMap.get curr name with
      | Some value -> Some value
      | None -> aux rest
      end
    | [] -> None
  in
  aux stack

let resolveAndGet name scope =
  let open Common.Option.Syntax in
  let%bind name = resolve name scope in
  let%bind value = get name scope in
  return (name, value)

let add bindings {stack; values} =
  let prefix = string_of_int (Js.Math.random_int 1 1000) in
  let top, values =
    let f (top, values) (name, value) =
      let uniqName = name ^ "$" ^ prefix in
      (StringMap.set top name uniqName,
       StringMap.set values uniqName (name, value))
    in
    Belt.List.reduce bindings (StringMap.empty, values) f
  in
  {stack = top::stack; values}

let rec bindings {values; _} =
  let f (uniqName, (name, value)) = uniqName, name, value in
  values |. StringMap.toList |. Belt.List.map f

let log {values; _} =
  let f name value =
    Js.log2 name value
  in
  StringMap.forEach values f

