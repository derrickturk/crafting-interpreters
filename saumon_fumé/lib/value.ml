type t =
  | Nil
  | Bool of bool
  | Num of float
  | Str of string
  | Fn of string * int * (t list -> (t, Error.t) result)
  [@@deriving show]

let truthy = function
  | Nil -> false
  | Bool false -> false
  | _ -> true

let pprint = function
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | Num f -> Printf.sprintf "%g" f (* TODO: nope, scientific is bad *)
  | Str s -> Printf.sprintf "\"%s\"" s
  | Fn (name, _, _) -> Printf.sprintf "<function %s>" name

let to_string = function
  | Str s -> s
  | v -> pprint v
