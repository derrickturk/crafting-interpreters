type unary_op =
  | Complement
  | Negate

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

val pprint_binary_op: binary_op -> string

module type Spec = sig
  type var
  val pprint_var: var -> string

  type 'a annot
  val pprint_annot: ('a -> string) -> 'a annot -> string

  (* TODO: fndef/block slot-count annot type; either () or int *)
end

module type S = sig
  type var
  type 'a annot

  val pprint_var: var annot -> string

  type expr =
    | Lit of Value.t
    | UnaryOp of unary_op * expr annot
    | BinaryOp of binary_op * expr annot * expr annot
    | Var of var annot

  val pprint_expr: expr annot -> string

  type stmt =
    | Expr of expr annot
    | Print of expr annot
    | VarDecl of var annot * expr annot option

  val pprint_stmt: stmt annot -> string

  type prog = stmt annot list

  val pprint_prog: prog -> string
end

module Make (Sp: Spec): S with type var = Sp.var and type 'a annot = 'a Sp.annot

module AsParsed: S with type var = string and type 'a annot = 'a Located.t

module AsResolved: S
  with type var = (string * int * int) and type 'a annot = 'a Located.t
