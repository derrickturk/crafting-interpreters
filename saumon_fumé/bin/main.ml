open Saumon_fume

let run src =
  let open Parser in
  let open Syntax in
  let print_error e =
    Printf.eprintf "Error: %s\n" (Error.show e);
    Out_channel.(flush stderr)
  in
  match parse_expr src with
    | Ok e ->
        print_endline (show_expr e);
        true
    | Error es ->
        List.iter print_error es;
        false

let rec run_interactive () =
  try
    Out_channel.(output_string stdout "> "; flush stdout);
    let line = read_line () in
    let _ = run line in
    run_interactive ()
  with End_of_file -> ()

let run_file path =
  let src = try
    In_channel.(with_open_text path input_all)
  with
    _ -> Printf.eprintf "Unable to read %s\n" path; exit 2
  in
  run src

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
