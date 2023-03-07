type ('a, 'e) t = ('a, 'e) result

val return: 'a -> ('a, 'e) t
val (let+): ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
val (and+): ('a, 'e) t -> ('b, 'e) t -> (('a * 'b), 'e) t
val (let*): ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
val (and*): ('a, 'e) t -> ('b, 'e) t -> (('a * 'b), 'e) t
