type t

(* TODO: builtins! *)
val global: Resolver.t -> t

val expand: t -> Resolver.resolve_frame -> t

val read: t -> Syntax.AsResolved.var -> Value.t

val write: t -> Syntax.AsResolved.var -> Value.t -> unit
