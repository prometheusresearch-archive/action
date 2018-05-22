(**
 * A computation which modifies the database.
 *)

type  error = [`DatabaseError of string | `QueryTypeError of string ]

type ('v, 'err) t constraint [> error] = 'err

type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t

type kind = [
  | `Update
  | `Create
  ]

val make : ('v -> (unit, 'err) Run.t) -> kind -> ('v, 'err) t

val execute : mutation : ('v, 'err) t -> 'v -> (unit, 'err) comp

val kind : ('v, 'err) t -> kind

val test : 'v -> bool
