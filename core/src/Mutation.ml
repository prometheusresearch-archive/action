type error = [`DatabaseError of string | `QueryTypeError of string ]

type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

type kind = [
  | `Update
  | `Create
  ]

type ('v, 'err) t = <
  kind : kind;
  execute : 'v -> (unit, 'err) comp;
> Js.t


external make : ('v -> (unit, 'err) comp) -> kind -> ('v, 'err) t =
  "MutationRepr" [@@bs.new] [@@bs.module "./MutationRepr"]

let execute ~mutation value =
  let execute = mutation##execute in
  execute value

let kind mutation =
  mutation##kind

let test_ : 'a -> bool = [%bs.raw {|
  function test(v) { return v instanceof MutationRepr.MutationRepr; }
|}]

let test x = test_ (Obj.magic x)
