open Located

module StrMap = Map.Make (String)

module SP = Syntax.AsParsed
module SR = Syntax.AsResolved

type t = {
  program: Syntax.AsResolved.prog;
  global_slots: int;
  builtins: Syntax.AsResolved.var Syntax.AsResolved.annot list;
}

type var_state =
  | Declared
  | Defined

(*
type fn_kind =
  | Function
*)

type resolve_frame = {
  slots: int;
  locals: (int * var_state) StrMap.t;
  (* kind: fn_kind option; *)
  parent: resolve_frame option;
}

type resolve_ctx = resolve_frame * Error.t list

type 'a resolve_state = ('a, resolve_ctx) State_monad.t

let init_global = {
  slots = 0;
  locals = StrMap.empty;
  (* kind = None; *)
  parent = None;
}

let is_global { parent; _ } = parent = None

let has_local { locals; _ } name = StrMap.mem name locals

let rec find_opt { parent; locals; _ } name =
  let open Option_monad in
  match StrMap.find_opt name locals with
    | Some (slot, _) -> Some (0, slot)
    | None ->
        let* p = parent in
        let+ (depth, slot) = find_opt p name in
        (depth + 1, slot)

let fail item loc =
  State_monad.modify (fun (f, errs) -> (f, { item; loc }::errs))

let put_frame f =
  State_monad.modify (fun (_, errs) -> (f, errs))

let declare { item; loc } =
  let open State_monad in
  let* (f, _) = get in
  if not (is_global f) && has_local f item
    then fail
      { Error.lexeme = None; details = AlreadyDefined item }
      loc
    else put_frame { f with
      slots = f.slots + 1;
      locals = StrMap.add item (f.slots, Declared) f.locals;
    }

let define { item; _ } =
  let open State_monad in
  let* (f, _) = get in
  match StrMap.find_opt item f.locals with
    | Some (n, _) -> put_frame
        { f with locals = StrMap.add item (n, Defined) f.locals }
    | None -> put_frame
        { f with
          slots = f.slots + 1;
          locals = StrMap.add item (f.slots, Defined) f.locals;
        }

let resolve { item; loc } =
  let open State_monad in
  let* (f, _) = get in
  match find_opt f item with
    | Some (depth, slot) -> return {
        item = (item, depth, slot);
        loc;
      }
    | None ->
        let* () = fail
          { Error.lexeme = None; details = UndefinedVariable item }
          loc
        in return {
          (* ugly, but this will always be thrown away because an
           *   error is produced *)
          item = (item, -1, -1);
          loc;
        }

let define_builtins names =
  let open State_monad in
  let define_builtin n =
    let v = { item = n; loc = BuiltIns } in
    let* () = define v in
    resolve v
  in
  traverse define_builtin names

let rec resolve_expr { item; loc } =
  let open State_monad in
  let+ item' = match item with
    | SP.Lit v -> return (SR.Lit v)
    | SP.UnaryOp (op, e) ->
        let+ e' = resolve_expr e in SR.UnaryOp (op, e')
    | SP.BinaryOp (op, e1, e2) ->
        let+ e1' = resolve_expr e1
        and+ e2' = resolve_expr e2
        in SR.BinaryOp (op, e1', e2')
    | SP.Var v -> let+ v' = resolve v in SR.Var v'
  in { item = item'; loc }

let resolve_stmt { item; loc } =
  let open State_monad in
  let+ item' = match item with
    | SP.Expr e ->
        let+ e' = resolve_expr e in SR.Expr e'
    | SP.Print e ->
        let+ e' = resolve_expr e in SR.Print e'
    | SP.VarDecl (v, init) ->
        let* () = declare v in
        let* init' = match init with
          | Some e ->
              let+ e' = resolve_expr e in Some e'
          | None -> return None
        in
        let* () = define v in
        let* v' = resolve v in
        return (SR.VarDecl (v', init'))
  in { item = item'; loc }

let resolve prog builtins =
  let open State_monad in
  let resolve_s =
    let+ builtins' = define_builtins builtins
    and+ prog' = traverse resolve_stmt prog in
    (builtins', prog')
  in
  match run (init_global, []) resolve_s with
    | ((builtins', prog'), (f, [])) -> Ok {
        program = prog';
        global_slots = f.slots;
        builtins = builtins';
      }
    | (_, (_, errs)) -> Error (List.rev errs)
