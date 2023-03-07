let rec eval_expr =
  let open Syntax in
  let open Value in
  let open Result_monad in
  let numeric_op = function
    | Sub -> (fun n1 n2 -> Num (n1 -. n2)), "-"
    | Mul -> (fun n1 n2 -> Num (n1 *. n2)), "*"
    | Div -> (fun n1 n2 -> Num (n1 /. n2)), "/"
    | Lt -> (fun n1 n2 -> Bool (n1 < n2)), "<"
    | LtEq -> (fun n1 n2 -> Bool (n1 <= n2)), "<="
    | Gt -> (fun n1 n2 -> Bool (n1 > n2)), ">="
    | GtEq -> (fun n1 n2 -> Bool (n1 >= n2)), ">="
    | _ -> failwith "internal error"
  in
  function
    | Lit (v, _) -> Ok v
    | UnaryOp (Complement, e, _) ->
        Result.map (fun v -> Bool (not (truthy v))) (eval_expr e)
    | UnaryOp (Negate, e, line) ->
        let* v = eval_expr e in
        begin match v with
          | Num n -> Ok (Num (-.n))
          | _ -> Error {
              Error.line = Some line;
              lexeme = Some "-";
              details = TypeError "operand must be number"
            }
        end
    | BinaryOp (Eq, e1, e2, _) ->
        let* lhs = eval_expr e1
        and* rhs = eval_expr e2
        in Ok (Bool (lhs = rhs))
    | BinaryOp (NotEq, e1, e2, _) ->
        let* lhs = eval_expr e1
        and* rhs = eval_expr e2
        in Ok (Bool (lhs != rhs))
    | BinaryOp (Add, e1, e2, line) ->
        let* lhs = eval_expr e1
        and* rhs = eval_expr e2 in
        begin match lhs, rhs with
          | Num n1, Num n2 -> Ok (Num (n1 +. n2))
          | Str s1, Str s2 -> Ok (Str (s1 ^ s2))
          | (Num _, Str _) | (Str _, Num _) -> Error {
              Error.line = Some line;
              lexeme = Some "+";
              details = TypeError "operands must have matching types"
            }
          | _ -> Error {
              Error.line = Some line;
              lexeme = Some "+";
              details = TypeError "operands must be numbers or strings"
            }
        end
    | BinaryOp ((Sub | Mul | Div | Lt | LtEq | Gt | GtEq) as o, e1, e2, line) ->
        let* lhs = eval_expr e1
        and* rhs = eval_expr e2 in
        let op_fn, op_lexeme = numeric_op o in
        begin match lhs, rhs with
          | Num n1, Num n2 -> Ok (op_fn n1 n2)
          | _ -> Error {
              Error.line = Some line;
              lexeme = Some op_lexeme;
              details = TypeError "operands must be numbers"
            }
        end
