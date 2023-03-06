let (* rec *) eval_expr = let open Syntax in function
  | Lit v -> Ok v
  | _ -> failwith "TODO"
