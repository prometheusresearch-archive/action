module Syntax = struct
  let return = Js.Promise.resolve

  module Let_syntax = struct

    let bind v f = Js.Promise.then_ f v
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
