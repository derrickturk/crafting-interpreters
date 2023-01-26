type token_kind =
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Not
  | NotEq
  | Eq
  | EqEq
  | Gt
  | GtEq
  | Lt
  | LtEq
  | Ident of string
  | StrLit of string
  | NumLit of float
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  [@@deriving show]

type token = { kind: token_kind; lexeme: string; line: int }
  [@@deriving show]

module Lexer = struct
  type t = { input: string; start_pos: int; next_pos: int; line: int }

  let init source = { input = source; start_pos = 0; next_pos = 0; line = 1 }

  let eof { input; next_pos; _ } = next_pos >= String.length input

  let peek { input; next_pos; _ } = try Some(input.[next_pos]) with _ -> None

  let peek_n { input; next_pos; _ } n = try
    Some(input.[next_pos + n])
  with _ -> None

  let consume l = try
    Some(({ l with next_pos = l.next_pos + 1 }, l.input.[l.next_pos]))
  with _ -> None

  let match_where l p = match consume l with
    | Some((_, c)) as r when p c -> r
    | _ -> None

  let match_char l c = match_where l (fun c' -> c = c')

  let lexeme { input; start_pos; next_pos; _ } =
    let len = next_pos - start_pos in String.sub input start_pos len

  let step l = { l with start_pos = l.next_pos }

  let token_here l f =
    let lm = lexeme l in
    (step l, Ok({ kind = f lm; lexeme = lm; line = l.line }))

  let error_here l details =
    (step l, Error({ Error.line = l.line; where = ""; details }))

  let rec strip_comment l = match consume l with
    | Some((_, '\n')) -> l
    | Some((l', _)) -> strip_comment l'
    | _ -> l

  let rec next l = match consume l with
    | None -> None

    | Some((l', ('\r' | '\t' | ' '))) -> next (step l')
    | Some((l', '\n')) -> next { (step l') with line = l'.line + 1 }

    | Some((l', '(')) -> Some(token_here l' (fun _ -> LParen))
    | Some((l', ')')) -> Some(token_here l' (fun _ -> RParen))
    | Some((l', '{')) -> Some(token_here l' (fun _ -> LBrace))
    | Some((l', '}')) -> Some(token_here l' (fun _ -> RBrace))
    | Some((l', ',')) -> Some(token_here l' (fun _ -> Comma))
    | Some((l', '.')) -> Some(token_here l' (fun _ -> Dot))
    | Some((l', '-')) -> Some(token_here l' (fun _ -> Minus))
    | Some((l', '+')) -> Some(token_here l' (fun _ -> Plus))
    | Some((l', ';')) -> Some(token_here l' (fun _ -> Semicolon))
    | Some((l', '*')) -> Some(token_here l' (fun _ -> Star))

    | Some((l', '!')) -> begin match match_char l' '=' with
        | Some((l'', _)) -> Some(token_here l'' (fun _ -> NotEq))
        | _ -> Some(token_here l' (fun _ -> Not))
      end
    | Some((l', '=')) -> begin match match_char l' '=' with
        | Some((l'', _)) -> Some(token_here l'' (fun _ -> EqEq))
        | _ -> Some(token_here l' (fun _ -> Eq))
      end
    | Some((l', '<')) -> begin match match_char l' '=' with
        | Some((l'', _)) -> Some(token_here l'' (fun _ -> LtEq))
        | _ -> Some(token_here l' (fun _ -> Lt))
      end
    | Some((l', '>')) -> begin match match_char l' '=' with
        | Some((l'', _)) -> Some(token_here l'' (fun _ -> GtEq))
        | _ -> Some(token_here l' (fun _ -> Gt))
      end

    | Some((l', '/')) -> begin match match_char l' '/' with
        | Some((l'', _)) -> next (strip_comment l'')
        | _ -> Some(token_here l' (fun _ -> Slash))
      end

    (* TODO: string and numeric literals *)

    | Some((l', c)) -> Some(error_here l' (UnexpectedCharacter c))
end

let lex source () =
  let open Lexer in
  let rec lex' l = match next l with
    | None -> Seq.Nil
    | Some ((l', tok)) -> Seq.Cons((tok, fun () -> lex' l'))
  in lex' (init source)
