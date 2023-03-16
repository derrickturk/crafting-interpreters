type ('a, 's) t = 's -> ('a * 's)

val return: 'a -> ('a, 's) t
val (let+): ('a, 's) t -> ('a -> 'b) -> ('b, 's) t
val (and+): ('a, 's) t -> ('b, 's) t -> (('a * 'b), 's) t
val (let*): ('a, 's) t -> ('a -> ('b, 's) t) -> ('b, 's) t
val (and*): ('a, 's) t -> ('b, 's) t -> (('a * 'b), 's) t

val get: ('s, 's) t
val put: 's -> (unit, 's) t
val modify: ('s -> 's) -> (unit, 's) t

val run: 's -> ('a, 's) t -> 'a * 's
val eval: 's -> ('a, 's) t -> 'a
val exec: 's -> ('a, 's) t -> 's

val traverse: ('a -> ('b, 's) t) -> 'a list -> ('b list, 's) t
val sequence: ('a -> (unit, 's) t) -> 'a list -> (unit, 's) t
