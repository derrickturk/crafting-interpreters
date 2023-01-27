type value =
  | Nil
  | Bool of bool
  | Num of float
  | Str of string
  [@@deriving show]

val pprint_value: value -> string
