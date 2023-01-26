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

val lex: string -> token Seq.t
