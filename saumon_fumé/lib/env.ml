type t = {
  frame: Value.t array;
  parent: t option;
}

let global r =
  let open Resolver in
  let open Value in
  let frame = Array.make (slots r.global_frame) Nil in
  { frame; parent = None }

let expand e globals =
  let global_slots = Resolver.slots globals in
  let len = Array.length e.frame in
  let gap = global_slots - len in
  if gap > 0
    then
      let extra = Array.make gap Value.Nil in
      { e with frame = Array.append e.frame extra }
    else e

let rec read e var = match e, var with
  | { frame; _ }, (_, 0, slot) ->
      frame.(slot)
  | { parent = Some p; _ }, (item, n, slot) when n > 0 ->
      read p (item, n - 1, slot)
  | _, (item, _, _) ->
      failwith ("internal error: bad lookup for " ^ item)

let rec write e var v = match e, var with
  | { frame; _ }, (_, 0, slot) ->
      frame.(slot) <- v
  | { parent = Some p; _ }, (item, n, slot) when n > 0 ->
      write p (item, n - 1, slot) v
  | _, (item, _, _) ->
      failwith ("internal error: bad lookup for " ^ item)
