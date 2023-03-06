type t =
  | Nil
  | Bool of bool
  | Num of float
  | Str of string
  [@@deriving show]

val pprint: t -> string
