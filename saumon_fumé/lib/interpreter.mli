val eval_expr: Syntax.AsResolved.expr Syntax.AsResolved.annot
  -> (Value.t, Error.t) result

val exec_stmt: Syntax.AsResolved.stmt Syntax.AsResolved.annot
  -> (unit, Error.t) result

val exec: Syntax.AsResolved.prog -> (unit, Error.t) result
