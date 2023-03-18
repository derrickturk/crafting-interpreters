type t =
  | Global of Value.t option array
  | Local of Value.t array * t

let global r =
  let open Resolver in
  let frame = Array.make (slots r.global_frame) None in
  Global frame

(* TODO: make this safer somehow? *)
let expand e globals = match e with
  | Local _ -> e
  | Global frame ->
      let global_slots = Resolver.slots globals in
      let len = Array.length frame in
      let gap = global_slots - len in
      if gap > 0
        then
          let extra = Array.make gap None in
          Global (Array.append frame extra)
        else e

let rec read e var = match e, var with
  | Local (frame, _), (_, 0, slot) ->
      Some frame.(slot)
  | Local (_, parent), (name, n, slot) when n > 0 ->
      read parent (name, n - 1, slot)
  | Global frame, (_, 0, slot) ->
      frame.(slot)
  | _, (item, _, _) ->
      failwith ("internal error: bad lookup for " ^ item)

let rec define e var v = match e, var with
  | Local (frame, _), (_, 0, slot) ->
      frame.(slot) <- v
  | Local (_, parent), (name, n, slot) when n > 0 ->
      define parent (name, n - 1, slot) v
  | Global frame, (_, 0, slot) ->
      frame.(slot) <- Some v
  | _, (item, _, _) ->
      failwith ("internal error: bad lookup for " ^ item)

let rec assign e var v = match e, var with
  | Local (frame, _), (_, 0, slot) ->
      frame.(slot) <- v;
      true
  | Local (_, parent), (name, n, slot) when n > 0 ->
      assign parent (name, n - 1, slot) v
  | Global frame, (_, 0, slot) ->
      begin match frame.(slot) with
        | Some _ ->
            frame.(slot) <- Some v;
            true
        | None -> false
      end
  | _, (item, _, _) ->
      failwith ("internal error: bad lookup for " ^ item)

let push e slots = Local (Array.make slots Value.Nil, e)

let pop = function
  | Global _ -> failwith "internal error: pop from global env"
  | Local (_, parent) -> parent
