type location =
  | BuiltIns
  | EndOfInput
  | LinePos of { line: int; pos: int }
  [@@deriving show]

val pprint_location: location -> string

type 'a t = { item: 'a; loc: location }
  [@@deriving show]

val pprint: ('a -> string) -> 'a t -> string
