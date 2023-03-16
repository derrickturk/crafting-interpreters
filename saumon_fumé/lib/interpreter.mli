val eval_expr: Env.t
  -> Syntax.AsResolved.expr Syntax.AsResolved.annot
  -> (Value.t, Error.t) result

val exec_stmt: Env.t
  -> Syntax.AsResolved.stmt Syntax.AsResolved.annot
  -> (unit, Error.t) result

val exec: Env.t -> Syntax.AsResolved.prog -> (unit, Error.t) result
