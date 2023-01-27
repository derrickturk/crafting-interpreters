type unary_op =
  | Complement
  | Negate
  [@@deriving show]

val pprint_unary_op: unary_op -> string

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | NotEq
  | Lt
  | LtEq
  | Gt
  | GtEq
  [@@deriving show]

val pprint_binary_op: binary_op -> string

type expr =
  | Lit of Interpreter.value
  | UnaryOp of unary_op * expr
  | BinaryOp of binary_op * expr * expr
  [@@deriving show]

val pprint_expr: expr -> string
