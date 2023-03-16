val parse_expr: string -> (Syntax.AsParsed.expr Located.t, Error.t list) result
val parse_stmt: string -> (Syntax.AsParsed.stmt Located.t, Error.t list) result
val parse: string -> (Syntax.AsParsed.prog, Error.t list) result
