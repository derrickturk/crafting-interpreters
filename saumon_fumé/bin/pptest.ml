open Saumon_fume.Syntax

let ex = BinaryOp(
  Add,
  BinaryOp(
    GtEq,
    UnaryOp(
      Complement,
      Lit(Bool(true))
    ),
    Lit(Nil)
  ),
  BinaryOp(
    Eq,
    Lit(Num(36.5)),
    Lit(Str("potato"))
  )
)

let pptest () = print_endline (pprint_expr ex)
