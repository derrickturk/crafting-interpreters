type t = {
  program: Syntax.AsResolved.prog;
  global_slots: int;
  builtins: Syntax.AsResolved.var Syntax.AsResolved.annot list;
}

val resolve: Syntax.AsParsed.prog -> string list -> (t, Error.t list) result
