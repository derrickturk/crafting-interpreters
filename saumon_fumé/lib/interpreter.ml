open Located

exception RetExn of Value.t

let rec eval_expr env { item; loc } =
  let open Syntax in
  let open Syntax.AsResolved in
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
  in match item with
    | Lit v -> Ok v
    | UnaryOp (Complement, e) ->
        Result.map (fun v -> Bool (not (truthy v))) (eval_expr env e)
    | UnaryOp (Negate, e) ->
        let* v = eval_expr env e in
        begin match v with
          | Num n -> Ok (Num (-.n))
          | _ -> Error {
              item = {
                Error.lexeme = Some "-";
                details = TypeError "operand must be number";
              };
              loc;
            }
        end
    | BinaryOp (Eq, e1, e2) ->
        let* lhs = eval_expr env e1
        and* rhs = eval_expr env e2
        in Ok (Bool (lhs = rhs))
    | BinaryOp (NotEq, e1, e2) ->
        let* lhs = eval_expr env e1
        and* rhs = eval_expr env e2
        in Ok (Bool (lhs != rhs))
    | BinaryOp (Add, e1, e2) ->
        let* lhs = eval_expr env e1
        and* rhs = eval_expr env e2 in
        begin match lhs, rhs with
          | Num n1, Num n2 -> Ok (Num (n1 +. n2))
          | Str s1, Str s2 -> Ok (Str (s1 ^ s2))
          | (Num _, Str _) | (Str _, Num _) -> Error {
              item = {
                lexeme = Some "+";
                details = TypeError "operands must have matching types"
              };
              loc;
            }
          | _ -> Error {
              item = {
                lexeme = Some "+";
                details = TypeError "operands must be numbers or strings"
              };
              loc;
            }
        end
    | BinaryOp ((Sub | Mul | Div | Lt | LtEq | Gt | GtEq) as o, e1, e2) ->
        let* lhs = eval_expr env e1
        and* rhs = eval_expr env e2 in
        let op_fn, op_lexeme = numeric_op o in
        begin match lhs, rhs with
          | Num n1, Num n2 -> Ok (op_fn n1 n2)
          | _ -> Error {
              item = {
                lexeme = Some op_lexeme;
                details = TypeError "operands must be numbers";
              };
              loc;
            }
        end
    | BinaryOp (And, e1, e2) ->
        let* lhs = eval_expr env e1 in
        if Value.truthy lhs
          then eval_expr env e2
          else Ok lhs
    | BinaryOp (Or, e1, e2) ->
        let* lhs = eval_expr env e1 in
        if Value.truthy lhs
          then Ok lhs
          else eval_expr env e2
    | Var { item = (name, _, _) as var; loc } ->
        begin match Env.read env var with
          | Some v -> Ok v
          | None -> Error {
              item = {
                lexeme = None;
                details = UndefinedVariable name;
              };
              loc;
            }
        end
    | Assign ({ item = (name, _, _) as var; loc}, e) ->
        let* v = eval_expr env e in
        if Env.assign env var v
          then Ok v
          else Error {
            item = {
              lexeme = None;
              details = UndefinedVariable name;
            };
            loc;
          }
    | Call (callee, args) ->
        let* fn = eval_expr env callee in
        match fn with
          | Fn (name, arity, f) ->
              let n_args = List.length args in
              if n_args != arity
                then
                  let msg = Printf.sprintf
                    "function %s expected %d arguments; received %d"
                    name arity n_args
                  in Error {
                    item = {
                      lexeme = None;
                      details = TypeError msg;
                    };
                    loc = callee.loc;
                  }
                else
                  let* args' = traverse (eval_expr env) args in
                  f args'
          | _ -> Error {
              item = {
                lexeme = None;
                details = TypeError "attempt to call non-callable value";
              };
              loc = callee.loc;
            }

let rec exec_stmt env { item; _ } =
  let open Syntax.AsResolved in
  let open Result_monad in
  let rec loop_while c s =
    let* v = eval_expr env c in
    if Value.truthy v
      then
        let* () = exec_stmt env s in
        loop_while c s
      else Ok ()
  in
  match item with
    | Expr e ->
        let+ _ = eval_expr env e in ()
    | IfElse (c, sif, selse) ->
        let* v = eval_expr env c in
        if Value.truthy v
          then exec_stmt env sif
          else begin match selse with
            | Some s -> exec_stmt env s
            | None -> Ok () 
          end
    | While (c, s) ->
        loop_while c s
    | Print e ->
        let+ v = eval_expr env e in
        print_endline (Value.to_string v)
    | Return e ->
        let+ v = eval_expr env e in
        raise (RetExn v)
    | Block (stmts, slots) ->
        let env' = Env.push env slots in
        sequence (exec_stmt env') stmts
    | VarDecl (v, None) ->
        Ok (Env.define env v.item Value.Nil)
    | VarDecl (v, Some init) ->
        let+ init' = eval_expr env init in
        Env.define env v.item init'
    | FunDef ({ item = (name, _, _); _ } as v, params, body, slots) ->
        let fn args =
          let env' = Env.push env slots in
          List.iter2 (fun p a -> Env.define env' p.item a) params args;
          try
            let* () = sequence (exec_stmt env') body in
            Ok Value.Nil
          with
            | RetExn v -> Ok v
        in
        Env.define env v.item (Value.Fn (name, List.length params, fn));
        Ok ()

let exec env = Result_monad.sequence (exec_stmt env)
