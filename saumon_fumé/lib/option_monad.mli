type 'a t = 'a option

val return: 'a -> 'a t
val (let+): 'a t -> ('a -> 'b) -> 'b t
val (and+): 'a t -> 'b t -> ('a * 'b) t
val (let*): 'a t -> ('a -> 'b t) -> 'b t
val (and*): 'a t -> 'b t -> ('a * 'b) t
