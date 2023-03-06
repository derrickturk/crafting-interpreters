type details =
  | ParseExpected of string
  | UnexpectedCharacter of char
  | UnterminatedStrLit
  [@@deriving show]

type t = { line: int option; lexeme: string option; details: details }
  [@@deriving show]
