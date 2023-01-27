type value =
  | Nil
  | Bool of bool
  | Num of float
  | Str of string
  [@@deriving show]

let pprint_value = function
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | Num f -> string_of_float f
  | Str s -> Printf.sprintf "\"%s\"" s
