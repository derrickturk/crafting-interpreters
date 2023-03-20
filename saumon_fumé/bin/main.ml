open Saumon_fume

let print_errors es =
  let print_error e =
    Printf.eprintf "Error: %s\n" (Error.pprint e);
    Out_channel.(flush stderr)
  in List.iter print_error es

let run_interactive () =
  let no_errors = ref true in
  let resolved = match Resolver.resolve [] Builtin.builtins with
    | Ok r -> r
    | _ -> failwith "internal error: bultin resolve failed" 
  in
  let global_frame = ref resolved.global_frame in
  let env = ref (Env.global resolved Builtin.builtins) in
  let run_line line =
    let open Interpreter in
    let open Parser in
    let open Resolver in
    let open Result_monad in
    let run_impl src =
      let* prog = parse src in
      let* resolved, global_frame' = resolve_incremental !global_frame prog in
      let env' = Env.expand !env global_frame' in
      let* () = Result.map_error (fun e -> [e]) (exec env' resolved) in
      global_frame := global_frame';
      env := env';
      Ok ()
    in
    match run_impl line with
      | Error es -> print_errors es; false
      | Ok () -> true
  in
  let rec go () =
    try
      Out_channel.(output_string stdout "> "; flush stdout);
      let line = read_line () in
      if not (run_line line) then no_errors := false;
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
  let open Interpreter in
  let open Parser in
  let open Resolver in
  let open Result_monad in
  let run_impl src =
    let* prog = parse src in
    let* resolved = resolve prog Builtin.builtins in
    let env = Env.global resolved Builtin.builtins in
    Result.map_error (fun e -> [e]) (exec env resolved.program)
  in
  match run_impl src with
    | Error es -> print_errors es; false
    | Ok () -> true

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
