module R = Js.Result

type ('v, 'errctx) t = ('v, 'errctx error) R.t

and 'errctx error = ('errctx * 'errctx list)

let return v = R.Ok v

let error ctx = R.Error (ctx, [])

let context ctx comp =
  match comp with
  | R.Ok v -> R.Ok v
  | R.Error (first, rest) -> R.Error (first, ctx::rest)

let toResult = function
  | R.Ok v -> R.Ok v
  | R.Error (first, _rest) -> R.Error first

module Syntax = struct

  let return = return
  let error = error
  let context = context

  module Let_syntax = struct

    let bind v f =
      match v with
      | R.Ok v -> f v
      | R.Error ctxs -> R.Error ctxs
  end
end

module List = struct
  let rec map ~f =
    let module Let_syntax = Syntax.Let_syntax in
    function
    | [] -> return []
    | x::xs ->
      let%bind x = f x in
      let%bind xs = map ~f xs in
      return (x::xs)

  let rec iter ~f =
    let module Let_syntax = Syntax.Let_syntax in
    function
    | [] -> return ()
    | x::xs ->
      let%bind () = f x in
      iter ~f xs

  let rec foldLeft ~f ~init:v =
    let module Let_syntax = Syntax.Let_syntax in
    function
    | [] -> return v
    | x::xs ->
      let%bind v = f v x in
      foldLeft ~f ~init:v xs
end

module Array = struct
  let map ~f v =
    let module Let_syntax = Syntax.Let_syntax in
    let v = Array.to_list v in
    let%bind v = List.map ~f v in
    return (Array.of_list v)
end
