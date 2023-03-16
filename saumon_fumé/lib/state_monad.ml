type ('a, 's) t = 's -> ('a * 's)

let return x = fun s -> (x, s)

let product m1 m2 = fun s ->
  let (x, s') = m1 s in
  let (y, s'') = m2 s' in
  ((x, y), s'')

let (let+) m f = fun s ->
  let (x, s') = m s in (f x, s')

let (and+) = product

let (let*) m f = fun s ->
  let (x, s') = m s in
  (f x) s'

let (and*) = product

let get = fun s -> (s, s)
let put = fun s' _ -> ((), s')
let modify = fun f s -> ((), f s)

let run s f = f s
let eval s f =
  let (x, _) = f s in x
let exec s f =
  let (_, s') = f s in s'

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
