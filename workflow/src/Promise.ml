module Syntax = struct
  let return = Js.Promise.resolve

  module Let_syntax = struct

    let bind v f = Js.Promise.then_ f v

    let both a b =
      let waitB a b = Js.Promise.resolve (a, b) in
      let waitA a = Js.Promise.then_ (waitB a) b in
      Js.Promise.then_ waitA a
  end
end

let all promises =
  let open Syntax in
  let%bind results =
    promises
    |> Array.of_list
    |> Js.Promise.all
  in return (Array.to_list results)

let return = Js.Promise.resolve
