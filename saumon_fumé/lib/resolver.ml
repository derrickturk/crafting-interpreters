open Located

module StrMap = Map.Make (String)

module SP = Syntax.AsParsed
module SR = Syntax.AsResolved

type var_state =
  | Declared
  | Defined

type fn_kind =
  | Function

type resolve_frame = {
  slots: int;
  locals: (int * var_state) StrMap.t;
  kind: fn_kind option;
  parent: resolve_frame option;
}

let init_global = {
  slots = 0;
  locals = StrMap.empty;
  kind = None;
  parent = None;
}

let is_global { parent; _ } = parent = None

let has_local { locals; _ } name = StrMap.mem name locals

type resolve_ctx = resolve_frame * Error.t list

type 'a resolve_state = ('a, resolve_ctx) State_monad.t

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

let define_builtins =
  State_monad.sequence (fun n -> define { item = n; loc = BuiltIns })

(*
let rec traverse_stmts f errs stmts =
  let rec go f errs rev_stmts = function
    | [] -> (f, errs, List.rev rev_stmts)
    | stmt::rest ->
        let f', errs', stmt' = resolve_stmt f errs stmt in
        go f' errs' (stmt'::rev_stmts)
  in go f errs [] stmts

and resolve_stmt f errs = function
  | { item = Expr e; loc } ->
      let f', errs', e' = resolve_expr f errs e
      in { item = Expr e'; loc }
  | { item = Print e; loc } ->
      let f', errs', e' = resolve_expr f errs e
      in { item = Print e'; loc }
  (*
  | { item = VarDecl (v, e); loc } ->
      match declare f v with
        | Some f' ->
  *)
*)

let resolve_expr { item; loc } = match item with
  | _ -> failwith "TODO"

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
        (* TODO TODO TODO *)
        return (SR.VarDecl ({ item = (v.item, 0, 0); loc = v.loc }, init'))
  in { item = item'; loc }

let resolve prog builtins =
  let open State_monad in
  let resolve_s =
    let* () = define_builtins builtins in
    traverse resolve_stmt prog
  in
  match run (init_global, []) resolve_s with
    | (prog', (_, [])) -> Ok prog'
    | (_, (_, errs)) -> Error (List.rev errs)
