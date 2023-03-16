module Parser = struct
  module L = Lexical

  open Located
  open Option_monad
  open Syntax
  open Syntax.AsParsed

  type t = {
    mutable input: (L.token Located.t, Error.t) result Seq.t;
    mutable errors: Error.t list
  }

  let init source = { input = Seq.memoize (L.lex source); errors = [] }

  let match_token p kind = match p.input () with
    | Cons (Ok tok, tl) when tok.item.kind == kind ->
        p.input <- tl;
        Some tok
    | _ -> None

  let match_token_opt p f = match p.input () with
    | Cons (Ok tok, tl) ->
        let+ v = f tok.item.kind in
        p.input <- tl;
        (v, tok)
    | _ -> None

  let require p f what = match p.input () with
    | Cons (Ok tok, tl) when f tok.item.kind ->
        p.input <- tl;
        Some tok
    | Cons (Ok tok, tl) ->
        let e = {
          Located.item = {
            Error.lexeme = Some tok.item.lexeme;
            details = ParseExpected what;
          };
          loc = tok.loc;
        } in
        p.input <- tl;
        p.errors <- e::p.errors;
        None
    | Cons (Error e, tl) ->
        p.input <- tl;
        p.errors <- e::p.errors;
        None
    | Nil ->
        let e = {
          Located.item = {
            Error.lexeme = None;
            details = ParseExpected what;
          };
          loc = EndOfInput;
        } in
        p.errors <- e::p.errors;
        None

  let require_kind p kind what = require p (fun k -> k == kind) what

  let fold_binary_op p expr_p op_fn =
    let rec go lhs = match match_token_opt p op_fn with
      | None -> Some lhs
      | Some (op, tok) ->
          let* rhs = expr_p p in
          go ({ item = BinaryOp (op, lhs, rhs); loc = tok.loc })
    in
    let* lhs = expr_p p in
    go lhs

  let rec expression p = equality p

  and equality p = fold_binary_op p comparison
    (function | EqEq -> Some Eq | NotEq -> Some NotEq | _ -> None)

  and comparison p = fold_binary_op p term begin
    function
      | Lt -> Some Lt | Gt -> Some Gt
      | LtEq -> Some LtEq | GtEq -> Some GtEq
      | _ -> None
  end

  and term p = fold_binary_op p factor
    (function | Plus -> Some Add | Minus -> Some Sub | _ -> None)

  and factor p = fold_binary_op p unary
    (function | Star -> Some Mul | Slash -> Some Div | _ -> None)

  and unary p =
    let unary_op = function
      | L.Not -> Some Complement
      | Minus -> Some Negate
      | _ -> None
    in
    match match_token_opt p unary_op with
      | Some (op, tok) ->
          let+ rhs = unary p in
          { item = UnaryOp (op, rhs); loc = tok.loc; }
      | None -> primary p

  and primary p =
    let literal =
      let open Value in
      function
        | L.Nil -> Some Nil
        | True -> Some (Bool true)
        | False -> Some (Bool false)
        | StrLit s -> Some (Str s)
        | NumLit n -> Some (Num n)
        | _ -> None
    in
    let var =
      function
        | L.Ident name -> Some name
        | _ -> None
    in
    match match_token_opt p literal with
      | Some (lit, tok) ->
          Some { item = Lit lit; loc = tok.loc }
      | None -> match match_token_opt p var with
          | Some (v, tok) ->
              Some { item = Var { item = v; loc = tok.loc }; loc = tok.loc }
          | None -> match match_token p LParen with
              | Some _ ->
                  let* e = expression p in
                  let* _ = require_kind p RParen "')'" in
                  Some e
              | None ->
                  let _ = require p (fun _ -> false) "an expression" in
                  None

  let rec match_kinds p fallback = function
    | [] -> fallback p
    | (kind, fn_rest)::tl -> match match_token p kind with
        | Some tok -> fn_rest p tok
        | None -> match_kinds p fallback tl

  let rec declaration p = match_kinds p statement [(L.Var, var_decl_rest)]

  and var_decl_rest p tok =
    let is_ident = function | L.Ident _ -> true | _ -> false in
    let* ident = require p is_ident "identifier" in
    let var = match ident.item.kind with
      | Ident name -> { item = name; loc = ident.loc }
      | _ -> failwith "internal error: non-ident matched as ident"
    in
    let* init = match match_token p Eq with
      | None -> Some None
      | Some _ ->
          (* this is super goofy... TODO test and improve *)
          let* e = expression p in
          Some (Some e)
    in
    let* _ = require_kind p Semicolon "';'" in
    Some { item = VarDecl (var, init); loc = tok.loc }

  and statement p = match_kinds p expression_statement
    (*
    [ (L.If, if_else_rest)
    ; (L.While, while_rest)
    ; (L.For, for_rest)
    *)
    [ (L.Print, print_rest)
    ]
    (*
    ; (L.Return, return_rest)
    ; (L.LBrace, block_rest)
    ]
    *)

  and expression_statement p =
    let* e = expression p in
    let* _ = require_kind p Semicolon "';'" in
    Some { item = Expr e; loc = e.loc }

  and print_rest p tok =
    let* e = expression p in
    let* _ = require_kind p Semicolon "';'" in
    Some { item = Print e; loc = tok.loc }

  let eof p = match p.input () with
    | Nil -> Some ()
    | Cons (Error e, tl) ->
        p.input <- tl;
        p.errors <- e::p.errors;
        None
    | Cons (Ok tok, tl) ->
        let e = {
          Located.item = {
            Error.lexeme = Some tok.item.lexeme;
            details = ParseExpected "end of input"
          };
          loc = tok.loc;
        } in
        p.input <- tl;
        p.errors <- e::p.errors;
        None

  let is_eof p =
    match p.input () with
      | Nil -> true
      | _ -> false

  let rec recover p =
    match p.input () with
      | Nil -> () (* eof, done *)
      | Cons (Error e, tl) ->
          (* report lexer error, advance, continue recovery *)
          p.errors <- e::p.errors;
          p.input <- tl;
          recover p
      | Cons (Ok tok, tl) -> match tok.item.kind with
          | Semicolon ->
              (* consume, done *)
              p.input <- tl
          | Class | Fun | Var | For | If | While | Print | Return ->
              (* hold here, done *)
              ()
          | _ ->
              (* advance, continue recovery *)
              p.input <- tl;
              recover p
end

let parse_expr source =
  let open Parser in
  let p = init source in
  match expression p with
    | None -> Error (List.rev p.errors)
    | Some e -> match eof p with
        | None -> Error (List.rev p.errors)
        | Some () -> Ok e

let parse_stmt source =
  let open Parser in
  let p = init source in
  match declaration p with
    | None -> Error (List.rev p.errors)
    | Some s -> match eof p with
        | None -> Error (List.rev p.errors)
        | Some () -> Ok s

let parse source =
  let open Parser in
  let rec go decls had_err p =
    if is_eof p
      then if had_err
        then Error (List.rev p.errors)
        else Ok (List.rev decls)
    else match declaration p with
      | Some decl -> go (decl::decls) had_err p
      | None ->
          recover p;
          go decls true p
  in go [] false (init source)
