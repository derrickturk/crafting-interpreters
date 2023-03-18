type t

(* TODO: builtins! *)
val global: Resolver.t -> t

val expand: t -> Resolver.resolve_frame -> t

val read: t -> Syntax.AsResolved.var -> Value.t option

val define: t -> Syntax.AsResolved.var -> Value.t -> unit

val assign: t -> Syntax.AsResolved.var -> Value.t -> bool

val push: t -> int -> t

val pop: t -> t
