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
    (step l, { kind = f lm; lexeme = lm; line = l.line })

  let rec next l = match consume l with
    | None -> None
    | Some((l', '(')) -> Some(token_here l' (fun _ -> LParen))
end

let lex source () =
  let rec lex' l = match next l with
    | None -> Seq.Nil
    | Some ((l', tok)) -> Seq.Cons((tok, fun () -> lex' l'))
  in lex' (init source)
