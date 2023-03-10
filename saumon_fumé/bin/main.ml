open Saumon_fume

let run src =
  let open Interpreter in
  let open Parser in
  let print_error e =
    Printf.eprintf "Error: %s\n" (Error.pprint e);
    Out_channel.(flush stderr)
  in
  match parse_expr src with
    | Error es ->
        List.iter print_error es;
        false
    | Ok e ->
        match eval_expr e with
          | Error e ->
              print_error e;
              false
          | Ok v ->
              print_endline (Value.pprint v);
              true

let run_interactive () =
  let no_errors = ref true in
  let rec go () =
    try
      Out_channel.(output_string stdout "> "; flush stdout);
      let line = read_line () in
      if not (run line) then no_errors := false;
      go ()
    with End_of_file ->
      print_endline "";
      !no_errors
  in go ()

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
        if not (run_interactive ()) then exit 1 else ()
    | [|_; file|] ->
        if not (run_file file) then exit 1 else ()
    | args ->
        let prog = try args.(0) with _ -> "saumon_fume" in
        Printf.eprintf "Usage: %s [source-file]\n" prog;
        exit 2
