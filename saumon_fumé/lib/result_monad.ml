type ('a, 'e) t = ('a, 'e) result

let return x = Ok x

let product r1 r2 = match r1, r2 with
  | (Ok x, Ok y) -> Ok (x, y)
  | (Error e, _) -> Error e
  | (_, Error e) -> Error e

let (let+) r f = Result.map f r
let (and+) = product
let (let*) = Result.bind
let (and*) = product

let traverse f xs =
  let rec go rev = function
    | [] -> return (List.rev rev)
    | hd::tl ->
        let* hd' = f hd in
        go (hd'::rev) tl
  in go [] xs

let rec sequence f = function
  | [] -> return ()
  | hd::tl ->
      let* () = f hd in
      sequence f tl
