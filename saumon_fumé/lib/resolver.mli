type resolve_frame

type t = {
  program: Syntax.AsResolved.prog;
  global_frame: resolve_frame;
  builtins: Syntax.AsResolved.var Syntax.AsResolved.annot list;
}

val slots: resolve_frame -> int

val resolve: Syntax.AsParsed.prog -> Builtin.t list -> (t, Error.t list) result

val resolve_incremental: resolve_frame
  -> Syntax.AsParsed.prog
  -> (Syntax.AsResolved.prog * resolve_frame, Error.t list) result
