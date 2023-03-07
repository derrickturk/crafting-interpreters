type t =
  | Nil
  | Bool of bool
  | Num of float
  | Str of string
  [@@deriving show]

let truthy = function
  | Nil -> false
  | Bool false -> false
  | _ -> true

let pprint = function
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | Num f -> Printf.sprintf "%g" f
  | Str s -> Printf.sprintf "\"%s\"" s
