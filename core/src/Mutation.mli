(**
 * A computation which modifies the database.
 *)

type  error = [`DatabaseError of string | `QueryTypeError of string ]

type ('v, 'err) t constraint [> error] = 'err

type ('v, 'err) comp = ('v, [> error ] as 'err) Run.t


val make : ('v -> (unit, 'err) Run.t) -> ('v, 'err) t

val execute : ('v, 'err) t -> 'v -> (unit, 'err) comp

val test : 'v -> bool
