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

type token = { kind: token_kind; lexeme: string }

module Lexer = struct
  type t = {
    input: string;
    start_pos: int;
    next_pos: int;
    line: int;
    line_char: int;
  }

  let init source = {
    input = source;
    start_pos = 0;
    next_pos = 0;
    line = 1;
    line_char = 1;
  }

  let peek_n { input; next_pos; _ } n = try
    Some(input.[next_pos + n])
  with _ -> None

  let consume l = try
    Some((
      { l with next_pos = l.next_pos + 1; line_char = l.line_char + 1 },
      l.input.[l.next_pos]
    ))
  with _ -> None

  let match_where l p = match consume l with
    | Some((_, c)) as r when p c -> r
    | _ -> None

  let match_char l c = match_where l (fun c' -> c = c')

  let rec match_while l p = match match_where l p with
    | Some((l', _)) -> match_while l' p
    | None -> l

  let lexeme { input; start_pos; next_pos; _ } =
    let len = next_pos - start_pos in String.sub input start_pos len

  let step l = { l with start_pos = l.next_pos }

  let here_loc l = 
    let open Located in
    LinePos { line = l.line; pos = l.line_char }

  let token_loc l =
    let open Located in
    let pos = l.line_char - (l.next_pos - l.start_pos) in
    LinePos { line = l.line; pos }

  let token_here l f =
    let open Located in
    let lm = lexeme l in
    (step l, Ok({ item = { kind = f lm; lexeme = lm }; loc = token_loc l }))

  let error_here l details =
    let open Located in
    let open Error in
    (step l, Error({ item = { lexeme = None; details }; loc = here_loc l }))

  let rec strip_comment l = match consume l with
    | Some((_, '\n')) ->
        { l with line = l.line + 1; line_char = 1 }
    | Some((l', _)) -> strip_comment l'
    | _ -> l

  let string_literal l =
    let l' = match_while l (fun c -> c != '"') in
    match consume l' with
    | Some((l'', '"')) -> token_here l''
        (fun lexeme -> StrLit (String.sub lexeme 1 (String.length lexeme - 2)))
    | _ -> error_here l UnterminatedStrLit

  let is_ascii_digit = function
    | '0'..'9' -> true
    | _ -> false

  let is_ident_begin = function
    | 'A'..'Z' | 'a'..'z' | '_' -> true
    | _ -> false

  let is_ident c = is_ident_begin c || is_ascii_digit c

  let num_literal l =
    let l' = match_while l is_ascii_digit in
    match consume l', peek_n l' 1 with
      | Some (l'', '.'), Some(d) when is_ascii_digit d ->
          token_here (match_while l'' is_ascii_digit)
            (fun lexeme -> NumLit (float_of_string lexeme))
      | _ -> token_here l' (fun lexeme -> NumLit (float_of_string lexeme))

  let ident_or_keyword l =
    let l' = match_while l is_ident in
    let kind = function
      | "and" -> And
      | "class" -> Class
      | "else" -> Else
      | "false" -> False
      | "for" -> For
      | "fun" -> Fun
      | "if" -> If
      | "nil" -> Nil
      | "or" -> Or
      | "print" -> Print
      | "return" -> Return
      | "super" -> Super
      | "this" -> This
      | "true" -> True
      | "var" -> Var
      | "while" -> While
      | l -> Ident(l)
    in token_here l' kind

  let rec next l = match consume l with
    | None -> None

    | Some((l', ('\r' | '\t' | ' '))) -> next (step l')
    | Some((l', '\n')) ->
        next { (step l') with line = l'.line + 1; line_char = 1 }

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

    | Some((l', '"')) -> Some(string_literal l')

    | Some((l', c)) when is_ascii_digit c -> Some(num_literal l')

    | Some((l', c)) when is_ident_begin c -> Some(ident_or_keyword l')

    | Some((l', c)) -> Some(error_here l' (UnexpectedCharacter c))
end

let lex source () =
  let open Lexer in
  let rec lex' l = match next l with
    | None -> Seq.Nil
    | Some ((l', tok)) -> Seq.Cons((tok, fun () -> lex' l'))
  in lex' (init source)
