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

module Lexer: sig
  type t
  val init: string -> t
  val eof: t -> bool
  val peek: t -> char option
  val consume: t -> (t * char) option
  val match_where: t -> (char -> bool) -> (t * char) option
  val match_char: t -> char -> (t * char) option
  val lexeme: t -> string
  val step: t -> t
  val token_here: t -> (string -> token_kind) -> t * token
  val next: t -> (t * token) option
end

val lex: string -> token Seq.t
