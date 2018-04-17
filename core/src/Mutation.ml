type error = [`DatabaseError of string | QueryTyper.error ]

type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

type ('v, 'err) t = <
  execute : 'v -> (unit, 'err) comp;
> Js.t


external make : ('v -> (unit, 'err) comp) -> ('v, 'err) t =
  "MutationRepr" [@@bs.new] [@@bs.module "./MutationRepr"]

let execute (mut : ('v, 'err) t) value =
  let execute = mut##execute in
  execute value

let test_ : 'a -> bool = [%bs.raw {|
  function test(v) { return v instanceof MutationRepr.MutationRepr; }
|}]

let test x = test_ (Obj.magic x)
