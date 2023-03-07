type details =
  | ParseExpected of string
  | TypeError of string
  | UnexpectedCharacter of char
  | UnterminatedStrLit
  [@@deriving show]

val pprint_details: details -> string

type t = { line: int option; lexeme: string option; details: details }
  [@@deriving show]

val pprint: t -> string
