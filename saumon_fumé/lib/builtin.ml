type t = string * Value.t

let builtins =
  let open Value in
  let b_clock = function
    | [] -> Ok (Num (Unix.gettimeofday ()))
    | _ -> failwith "internal error: bad builtin"
  in
  let b_readline = function
    | [] -> Ok (Str (read_line ()))
    | _ -> failwith "internal error: bad builtin"
  in
  let b_to_string = function
    | [v] -> Ok (Str (to_string v))
    | _ -> failwith "internal error: bad builtin"
  in
  [ ("clock", Fn ("clock", 0, b_clock))
  ; ("read_line", Fn ("read_line", 0, b_readline))
  ; ("to_string", Fn ("to_string", 1, b_to_string))
  ]
