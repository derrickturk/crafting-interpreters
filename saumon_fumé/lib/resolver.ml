open Located

module StrMap = Map.Make (String)

module SP = Syntax.AsParsed
module SR = Syntax.AsResolved

type var_state =
  | Declared
  | Defined
  | Deferred

type fn_kind =
  | Function

type resolve_frame = {
  slots: int;
  locals: (int * var_state) StrMap.t;
  kind: fn_kind option;
  parent: resolve_frame option;
}

let slots { slots; _ } = slots

type resolve_result =
  | Known of int * int * var_state
  | Unknown of int * int

let init_global = {
  slots = 0;
  locals = StrMap.empty;
  kind = None;
  parent = None;
}

type t = {
  program: Syntax.AsResolved.prog;
  global_frame: resolve_frame;
  builtins: Syntax.AsResolved.var Syntax.AsResolved.annot list;
}

let is_global { parent; _ } = parent = None

let find_name frame name =
  let rec go depth ({ slots; locals; parent; _ } as f) name =
    match StrMap.find_opt name locals with
      | Some (slot, state) -> f, Known (depth, slot, state)
      | None ->
          match parent with
            | Some p ->
                begin match go (depth + 1) p name with
                  | (_, (Known (_, _, _) as r)) ->
                      f, r
                  | (p', (Unknown (_, _) as r)) ->
                      { f with parent = Some p' }, r
                end
            | None -> { f with
                slots = slots + 1;
                locals = StrMap.add name (slots, Deferred) locals;
              }, Unknown (depth, slots)
  in go 0 frame name

let fail item loc =
  State_monad.modify (fun (f, errs) -> (f, { item; loc }::errs))

let put_frame f =
  State_monad.modify (fun (_, errs) -> (f, errs))

let push_frame kind =
  let child_frame f = {
      slots = 0;
      locals = StrMap.empty;
      kind;
      parent = Some f;
  } in
  State_monad.modify (fun (f, errs) -> (child_frame f, errs))

let pop_frame =
  let parent_frame = function
    | { parent = Some p; _ } -> p
    | _ -> failwith "internal error: pop from global frame"
  in
  State_monad.modify (fun (f, errs) -> (parent_frame f, errs))

(* TODO: I don't think I've factored any of this correctly, at all,
 *   as evidenced by the fact that I never seem to actually match
 *   on the var_state variants *)

let declare { item; loc } =
  let open State_monad in
  let* (f, _) = get in
  if is_global f
    then match StrMap.find_opt item f.locals with
      | Some (n, _) ->
          put_frame { f with
            locals = StrMap.add item (n, Declared) f.locals;
          }
      | None ->
          put_frame { f with
            slots = f.slots + 1;
            locals = StrMap.add item (f.slots, Declared) f.locals;
          }
    else match StrMap.find_opt item f.locals with
      | Some _ -> fail
          { Error.lexeme = None; details = AlreadyDefined item }
          loc
      | None ->
          put_frame { f with
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
  let (f', result) = find_name f item in
  let* () = put_frame f' in
  return begin match result with
    | Known (depth, slot, _) -> {
        item = (item, depth, slot);
        loc;
      }
    | Unknown (depth, slot) -> {
        item = (item, depth, slot);
        loc;
      }
  end

let local_slots =
  let open State_monad in
  let* (f, _) = get in
  return f.slots

let local_kind =
  let open State_monad in
  let* (f, _) = get in
  return f.kind

let define_builtins builtins =
  let open State_monad in
  let define_builtin (name, _) =
    let v = { item = name; loc = BuiltIns } in
    let* () = define v in
    resolve v
  in
  traverse define_builtin builtins

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
    | SP.Assign (v, e) ->
        let+ v' = resolve v
        and+ e' = resolve_expr e in
        SR.Assign (v', e')
    | SP.Call (e, args) ->
        let+ e' = resolve_expr e
        and+ args' = traverse resolve_expr args in
        SR.Call (e', args')
  in { item = item'; loc }

let rec resolve_stmt { item; loc } =
  let open State_monad in
  let+ item' = match item with
    | SP.Expr e ->
        let+ e' = resolve_expr e in SR.Expr e'
    | SP.IfElse (c, sif, selse) ->
        let+ c' = resolve_expr c
        and+ sif' = resolve_stmt sif
        and+ selse' = match selse with
          | Some s ->
              let+ s' = resolve_stmt s in Some s'
          | None -> return None
        in SR.IfElse (c', sif', selse')
    | SP.While (c, body) ->
        let+ c' = resolve_expr c
        and+ body' = resolve_stmt body
        in SR.While (c', body')
    | SP.Print e ->
        let+ e' = resolve_expr e in SR.Print e'
    | SP.Return e ->
        let* kind = local_kind in
        let* () = match kind with
          | Some Function -> return ()
          | _ -> fail {
              Error.lexeme = Some "return";
              details = InvalidReturn;
            } loc
        in
        let+ e' = resolve_expr e in SR.Return e'
    | SP.Block (body, ()) ->
        let* () = push_frame None in
        let* body' = traverse resolve_stmt body in
        let* slots = local_slots in
        let* () = pop_frame in
        return (SR.Block (body', slots))
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
    | SP.FunDef (v, params, body, ()) ->
        let* () = declare v in
        let* () = push_frame (Some Function) in
        let* () = sequence define params in
        let* params' = traverse resolve params in
        let* body' = traverse resolve_stmt body in
        let* slots = local_slots in
        let* () = pop_frame in
        let* () = define v in
        let* v' = resolve v in
        return (SR.FunDef (v', params', body', slots))
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
        global_frame = f;
        builtins = builtins';
      }
    | (_, (_, errs)) -> Error (List.rev errs)

let resolve_incremental global prog =
  let open State_monad in
  match run (global, []) (traverse resolve_stmt prog) with
    | (prog', (f, [])) -> Ok (prog', f)
    | (_, (_, errs)) -> Error (List.rev errs)
