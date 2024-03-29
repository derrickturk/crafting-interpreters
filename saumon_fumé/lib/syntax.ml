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
  | And
  | Or

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
  | And -> "and"
  | Or -> "or"

module type Spec = sig
  type var
  val pprint_var: var -> string

  type scope_info

  type 'a annot
  val pprint_annot: ('a -> string) -> 'a annot -> string
end

module type S = sig
  type var
  type scope_info
  type 'a annot

  val pprint_var: var annot -> string

  type expr =
    | Lit of Value.t
    | UnaryOp of unary_op * expr annot
    | BinaryOp of binary_op * expr annot * expr annot
    | Var of var annot
    | Assign of var annot * expr annot
    | Call of expr annot * expr annot list

  val pprint_expr: expr annot -> string

  type stmt =
    | Expr of expr annot
    | IfElse of expr annot * stmt annot * stmt annot option 
    | While of expr annot * stmt annot
    | Print of expr annot
    | Return of expr annot option
    | Block of stmt annot list * scope_info
    | VarDecl of var annot * expr annot option
    | FunDef of var annot * var annot list * stmt annot list * scope_info

  val pprint_stmt: stmt annot -> string

  type prog = stmt annot list

  val pprint_prog: prog -> string
end

module Make (Sp: Spec): S
with type var = Sp.var
and type scope_info = Sp.scope_info
and type 'a annot = 'a Sp.annot = struct
  type var = Sp.var
  type 'a annot = 'a Sp.annot

  type scope_info = Sp.scope_info

  let pprint_var = Sp.pprint_annot Sp.pprint_var

  type expr =
    | Lit of Value.t
    | UnaryOp of unary_op * expr annot
    | BinaryOp of binary_op * expr annot * expr annot
    | Var of var annot
    | Assign of var annot * expr annot
    | Call of expr annot * expr annot list

  let rec pprint_expr' = function
    | Lit v -> Value.pprint v
    | UnaryOp(op, e) ->
        let e' = pprint_expr e in
        Printf.sprintf "%s%s" (pprint_unary_op op) e'
    | BinaryOp(op, lhs, rhs) ->
        let lhs' = pprint_expr lhs in
        let rhs' = pprint_expr rhs in
        Printf.sprintf "(%s %s %s)" lhs' (pprint_binary_op op) rhs'
    | Var v -> pprint_var v
    | Assign (v, e) ->
        let v' = pprint_var v in
        let e' = pprint_expr e in
        Printf.sprintf "(%s = %s)" v' e'
    | Call (callee, args) ->
        let callee' = pprint_expr callee in
        let args' = String.concat ", " (List.map pprint_expr args) in
        Printf.sprintf "(%s)(%s)" callee' args'
  and pprint_expr e = Sp.pprint_annot pprint_expr' e

  type stmt =
    | Expr of expr annot
    | IfElse of expr annot * stmt annot * stmt annot option 
    | While of expr annot * stmt annot
    | Print of expr annot
    | Return of expr annot option
    | Block of stmt annot list * scope_info
    | VarDecl of var annot * expr annot option
    | FunDef of var annot * var annot list * stmt annot list * scope_info

  let rec pprint_stmt' = function
    | Expr e -> pprint_expr e ^ ";"
    | IfElse (e, s1, s2) ->
        let els = match s2 with
          | None -> ""
          | Some s -> " else " ^ pprint_stmt s
        in
        "if (" ^ pprint_expr e ^ ") " ^ pprint_stmt s1 ^ els
    | While (e, s) ->
        "while (" ^ pprint_expr e ^ ") " ^ pprint_stmt s
    | Print e -> "print " ^ pprint_expr e ^ ";"
    | Return None -> "return;"
    | Return (Some e) -> "return " ^ pprint_expr e ^ ";"
    | Block (stmts, _) ->
        "{\n" ^ String.concat "\n" (List.map pprint_stmt stmts) ^ "\n}"
    | VarDecl (v, None) -> "var " ^ pprint_var v ^ ";"
    | VarDecl (v, Some e) -> "var " ^ pprint_var v ^ " = " ^ pprint_expr e ^ ";"
    | FunDef (name, params, body, _) ->
        let name' = pprint_var name in
        let params' = String.concat ", " (List.map pprint_var params) in
        let body' = String.concat "\n" (List.map pprint_stmt body) in
        Printf.sprintf "%s (%s) {\n%s\n}" name' params' body'
  and pprint_stmt s = Sp.pprint_annot pprint_stmt' s

  type prog = stmt annot list

  let pprint_prog p =
    String.concat "" (List.map (fun s -> pprint_stmt s ^ "\n") p)
end

module AsParsed = Make (struct
  type var = string
  let pprint_var v = v

  type scope_info = unit

  type 'a annot = 'a Located.t
  let pprint_annot = Located.pprint
end)

module AsResolved = Make (struct
  type var = string * int * int
  let pprint_var (name, _, _) = name

  type scope_info = int

  type 'a annot = 'a Located.t
  let pprint_annot = Located.pprint
end)
