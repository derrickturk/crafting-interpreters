type 'a t = 'a option

let return x = Some x

let product o1 o2 = match o1, o2 with
  | (Some x, Some y) -> Some (x, y)
  | _ -> None

let (let+) o f = Option.map f o
let (and+) = product
let (let*) = Option.bind
let (and*) = product
