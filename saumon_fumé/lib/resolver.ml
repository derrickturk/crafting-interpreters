open Located

(* type resolve_ctx = unit *)

let resolve _ =
  let open Error in
  Error [{ item = { lexeme = None; details = UnterminatedStrLit }; loc = EndOfInput }]
