open Saumon_fume

let print_token_or_err = function
  | Ok t -> print_endline (Lexical.show_token t)
  | Error e -> print_endline (Error.show e)

let rec run_interactive () =
  try
    let line = read_line () in
    Seq.iter print_token_or_err (Lexical.lex line);
    run_interactive ()
  with End_of_file -> ()

let run_file path =
  let src = try
    In_channel.(with_open_text path input_all)
  with
    _ -> Printf.eprintf "Unable to read %s\n" path; exit 2
  in
  let had_error = ref false in
  let for_token = function
    | Ok(tok) -> print_endline (Lexical.show_token tok)
    | Error(e) ->
        Printf.eprintf "Error: %s\n" (Error.show e);
        had_error := true
  in
  Seq.iter for_token (Lexical.lex src);
  not !had_error

let () =
  match Sys.argv with
    | [|_|] ->
        run_interactive ()
    | [|_; file|] ->
        if not (run_file file) then exit 1 else ()
    | args ->
        let prog = try args.(0) with _ -> "saumon_fume" in
        Printf.eprintf "Usage: %s [source-file]\n" prog;
        exit 2
