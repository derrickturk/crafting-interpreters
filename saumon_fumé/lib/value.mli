type t =
  | Nil
  | Bool of bool
  | Num of float
  | Str of string
  | Fn of string * int * (t list -> (t, Error.t) result)
  [@@deriving show]

val truthy: t -> bool

val pprint: t -> string

val to_string: t -> string
