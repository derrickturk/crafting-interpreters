type unary_op =
  | Complement
  | Negate
  [@@deriving show]

let pprint_unary_op = function
  | Complement -> "!"
  | Negate -> "-"

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

let pprint_binary_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "=="
  | NotEq -> "!="
  | Lt -> "<"
  | LtEq -> "<="
  | Gt -> ">"
  | GtEq -> ">="

type expr =
  | Lit of Value.t
  | UnaryOp of unary_op * expr
  | BinaryOp of binary_op * expr * expr
  [@@deriving show]

let rec pprint_expr = function
  | Lit v -> Value.pprint v
  | UnaryOp(op, e) -> Printf.sprintf "%s%s" (pprint_unary_op op) (pprint_expr e)
  | BinaryOp(op, lhs, rhs) -> Printf.sprintf "%s %s %s"
      (pprint_expr lhs) (pprint_binary_op op) (pprint_expr rhs)
