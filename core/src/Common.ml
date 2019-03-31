module StringMap = Belt.Map.String
module MutStringMap = Belt.MutableMap.String

(**
 * An extension to stdlib - result type which can represent a value or a
 * failure.
 *)
module Result = struct
  include Js.Result

  let ignore = function
    | Ok _ -> Ok ()
    | Error err -> Error err

  let ofOption ~err = function
    | None -> Error err
    | Some v -> Ok v

  module Syntax = struct
    let return v = Ok v
    let error err = Error err

    module Let_syntax = struct
      let bind v ~f = match v with
      | Ok v -> f v
      | Error err -> Error err
    end
  end

  module List = struct
    let rec map ~f =
      let open Syntax in
      function
      | [] -> return []
      | x::xs ->
        let%bind x = f x in
        let%bind xs = map ~f xs in
        return (x::xs)

    let rec foldLeft ~f ~init:v =
      let open Syntax in
      function
      | [] -> return v
      | x::xs ->
        let%bind v = f v x in
        foldLeft ~f ~init:v xs
  end

  module Array = struct
    let map ~f v =
      let open Syntax in
      let v = Array.to_list v in
      let%bind v = List.map ~f v in
      return (Array.of_list v)
  end
end

(**
 * An extension to stdlib - option type which can represent either a value or an
 * absence of it.
 *)
module Option = struct
  include Js.Option

  module List = struct

    let rec filterNone = function
      | [] -> []
      | Some x::xs -> x::(filterNone xs)
      | None::xs -> filterNone xs

  end

  module Syntax = struct
    let return v = Some v

    module Let_syntax = struct
      let bind v ~f = match v with
      | Some v -> f v
      | None -> None
    end
  end


  let alt a b = match a with
  | Some a -> Some a
  | None -> b

end

