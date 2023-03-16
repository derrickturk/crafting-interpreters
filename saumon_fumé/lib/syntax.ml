type unary_op =
  | Complement
  | Negate

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

module type Spec = sig
  type var
  val pprint_var: var -> string

  type 'a annot
  val pprint_annot: ('a -> string) -> 'a annot -> string
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

module Make (Sp: Spec): S
with type var = Sp.var and type 'a annot = 'a Sp.annot = struct
  type var = Sp.var
  type 'a annot = 'a Sp.annot

  let pprint_var = Sp.pprint_annot Sp.pprint_var

  type expr =
    | Lit of Value.t
    | UnaryOp of unary_op * expr annot
    | BinaryOp of binary_op * expr annot * expr annot
    | Var of var annot

  let rec pprint_expr' = function
    | Lit v -> Value.pprint v
    | UnaryOp(op, e) ->
        let e' = pprint_expr e in
        Printf.sprintf "%s%s" (pprint_unary_op op) e'
    | BinaryOp(op, lhs, rhs) ->
        let lhs' = pprint_expr lhs in
        let rhs' = pprint_expr rhs in
        Printf.sprintf "%s %s %s" lhs' (pprint_binary_op op) rhs'
    | Var v -> pprint_var v
  and pprint_expr e = Sp.pprint_annot pprint_expr' e

  type stmt =
    | Expr of expr annot
    | Print of expr annot
    | VarDecl of var annot * expr annot option

  let rec pprint_stmt' = function
    | Expr e -> pprint_expr e ^ ";"
    | Print e -> "print " ^ pprint_expr e ^ ";"
    | VarDecl (v, None) -> "var " ^ pprint_var v ^ ";"
    | VarDecl (v, Some e) -> "var " ^ pprint_var v ^ " = " ^ pprint_expr e ^ ";"
  and pprint_stmt s = Sp.pprint_annot pprint_stmt' s

  type prog = stmt annot list

  let pprint_prog p =
    String.concat "" (List.map (fun s -> pprint_stmt s ^ "\n") p)
end

module AsParsed = Make (struct
  type var = string
  let pprint_var v = v

  type 'a annot = 'a Located.t
  let pprint_annot = Located.pprint
end)

module AsResolved = Make (struct
  type var = string * int * int
  let pprint_var (name, _, _) = name

  type 'a annot = 'a Located.t
  let pprint_annot = Located.pprint
end)
