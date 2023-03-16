type location =
  | BuiltIns
  | EndOfInput
  | LinePos of { line: int; pos: int }
  [@@deriving show]

let pprint_location = function
  | BuiltIns -> "[builtins]"
  | EndOfInput -> "[end of input]"
  | LinePos { line; pos }  ->
      "[line " ^ string_of_int line ^ ", char " ^ string_of_int pos ^ "]"

type 'a t = { item: 'a; loc: location }
  [@@deriving show]

let pprint pp { item; _ } = pp item
