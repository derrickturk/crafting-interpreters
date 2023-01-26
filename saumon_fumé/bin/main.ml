open Saumon_fume

let print_token_or_err = function
  | Ok t -> print_endline (Lexical.show_token t)
  | Error e -> print_endline (Error.show e)

let rec lex_loop () =
  try
    let line = read_line () in
    Seq.iter print_token_or_err (Lexical.lex line);
    lex_loop ()
  with End_of_file -> ()

let () = lex_loop ()
