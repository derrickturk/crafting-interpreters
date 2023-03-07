module Parser = struct
  module L = Lexical

  type t = {
    mutable input: (L.token, Error.t) result Seq.t;
    mutable errors: Error.t list
  }

  let init source = { input = Seq.memoize (L.lex source); errors = [] }

  let match_token p kind = match p.input () with
    | Cons (Ok tok, tl) when tok.kind == kind ->
        p.input <- tl;
        Some tok
    | _ -> None

  let match_token_opt p f = match p.input () with
    | Cons (Ok tok, tl) -> begin match f tok.kind with
        | Some v ->
            p.input <- tl;
            Some (v, tok)
        | None -> None
      end
    | _ -> None

  let require p f what = match p.input () with
    | Cons (Ok tok, tl) when f tok.kind ->
        p.input <- tl;
        Some tok
    | Cons (Ok tok, tl) ->
        let e = {
          Error.line = Some tok.line;
          lexeme = Some tok.lexeme;
          details = ParseExpected what
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
          Error.line = None;
          lexeme = None;
          details = ParseExpected what
        } in
        p.errors <- e::p.errors;
        None

  let fold_binary_op p expr_p op_fn =
    let open Syntax in
    let rec go lhs = match match_token_opt p op_fn with
      | None -> Some lhs
      | Some (op, tok) ->
          match expr_p p with
            | None -> None
            | Some rhs -> go (BinaryOp (op, lhs, rhs, tok.line))
    in
    match expr_p p with
      | None -> None
      | Some lhs -> go lhs

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
    let open Syntax in
    let unary_op = function
      | L.Not -> Some Complement
      | Minus -> Some Negate
      | _ -> None
    in
    match match_token_opt p unary_op with
      | Some (op, tok) -> begin
          match unary p with
            | Some rhs -> Some (UnaryOp (op, rhs, tok.line))
            | None -> None
        end
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
    let open Syntax in
    match match_token_opt p literal with
      | Some (lit, tok) -> Some (Lit (lit, tok.line))
      | None -> match match_token p LParen with
          | Some _ ->
              let e = expression p in
              begin match require p (fun k -> k == RParen) "')'" with
                | Some _ -> e
                | None -> None
              end
          | None ->
              let _ = require p (fun _ -> false) "an expression" in
              None

  let eof p = match p.input () with
    | Nil -> Some ()
    | Cons (Error e, tl) ->
        p.input <- tl;
        p.errors <- e::p.errors;
        None
    | Cons (Ok tok, tl) ->
        let e = {
          Error.line = Some tok.line;
          lexeme = Some tok.lexeme;
          details = ParseExpected "end of input"
        } in
        p.input <- tl;
        p.errors <- e::p.errors;
        None
end

let parse_expr source =
  let open Parser in
  let p = init source in
  match expression p with
    | None -> Error (List.rev p.errors)
    | Some e -> match eof p with
        | None -> Error (List.rev p.errors)
        | Some () -> Ok e
