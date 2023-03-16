type details =
  | AlreadyDefined of string
  | InvalidReturn
  | ParseExpected of string
  | TypeError of string
  | UnexpectedCharacter of char
  | UnterminatedStrLit
  [@@deriving show]

val pprint_details: details -> string

type error = { lexeme: string option; details: details }

val pprint_error: error -> string

type t = error Located.t

val pprint: t -> string
