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
  | Lit of Value.t * int
  | UnaryOp of unary_op * expr * int
  | BinaryOp of binary_op * expr * expr * int
  [@@deriving show]

let rec pprint_expr = function
  | Lit (v, _) -> Value.pprint v
  | UnaryOp(op, e, _) -> Printf.sprintf "%s%s" (pprint_unary_op op) (pprint_expr e)
  | BinaryOp(op, lhs, rhs, _) -> Printf.sprintf "%s %s %s"
      (pprint_expr lhs) (pprint_binary_op op) (pprint_expr rhs)
