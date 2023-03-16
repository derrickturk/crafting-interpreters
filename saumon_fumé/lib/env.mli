type t

(* TODO: builtins! *)
val global: Resolver.t -> t

val expand: t -> Resolver.t -> t

val read: t -> Syntax.AsResolved.var -> Value.t

val write: t -> Syntax.AsResolved.var -> Value.t -> unit
