type t

(* TODO: builtins! *)
val global: Resolver.t -> t

val expand: t -> Resolver.resolve_frame -> t

val read: t -> Syntax.AsResolved.var -> Value.t option

val write: t -> Syntax.AsResolved.var -> Value.t -> unit

val push: t -> int -> t
