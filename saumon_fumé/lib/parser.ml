module Parser = struct
  module L = Lexical

  type t = { input: (L.token, Error.t) result Seq.t; errors: Error.t list }

  let init source = { input = Seq.memoize (L.lex source); errors = [] }

  let match_token p pred = match p.input () with
    | Cons (Ok tok, tl) when pred tok.kind -> Some (tok, { p with input = tl })
    | _ -> None

  let match_token_opt p f = match p.input () with
    | Nil -> None
    | Cons (Ok tok, tl) -> match f tok with
        | Some v -> Some (v, { p with input = tl })
        | None -> None

  (*
  let fold_binary_op p expr_p op_fn =
    let rec go lhs p = match match_token_opt p op_fn with
      | None -> (Some lhs, p)
      | Some (op, p') ->
          match expr_p p with
            | None -> (None, p')
            | Some rhs -> go (BinaryOp (op, lhs, rhs)) p'
  *)

  (* let expression p = equality p *)

  let boof { input; _ } = ignore input; ()
end

let parse_expression source =
  let p = Parser.init source
   in Parser.boof p; Error (List.rev p.errors)
