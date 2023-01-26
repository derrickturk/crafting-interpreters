type details =
  | UnexpectedCharacter of char
  | UnterminatedStrLit
  [@@deriving show]

type t = { line: int; where: string; details: details }
  [@@deriving show]
