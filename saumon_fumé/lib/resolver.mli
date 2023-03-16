type t = {
  program: Syntax.AsResolved.prog;
  global_slots: int;
  builtins: Syntax.AsResolved.var Syntax.AsResolved.annot list;
}

type resolve_ctx;

val resolve: Syntax.AsParsed.prog -> string list -> (t, Error.t list) result
