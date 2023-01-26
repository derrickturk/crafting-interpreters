open Saumon_fume.Lexical

let rec lex_loop () =
  try
    let line = read_line () in
    Seq.iter (fun t -> print_endline (show_token t)) (lex line);
    lex_loop ()
  with End_of_file -> ()

let () = lex_loop ()
