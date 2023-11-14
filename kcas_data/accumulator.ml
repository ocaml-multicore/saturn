open Kcas

let n_way_max = Domain.recommended_domain_count () |> Bits.ceil_pow_2
let n_way_default = n_way_max |> Int.min 8

type t = int Loc.t array

let make ?n_way n =
  let n_way =
    match n_way with
    | None -> n_way_default
    | Some n_way -> n_way |> Int.min n_way_max |> Bits.ceil_pow_2
  in
  let a = Loc.make_array ~mode:Mode.lock_free n_way 0 in
  Loc.set (Array.unsafe_get a 0) n;
  a

let n_way_of = Array.length

let get_self a =
  let h = (Domain.self () :> int) in
  (* TODO: Consider mixing the bits of [h] to get better distribution *)
  Array.unsafe_get a (h land (Array.length a - 1))

module Xt = struct
  let add ~xt a n = if n <> 0 then Xt.fetch_and_add ~xt (get_self a) n |> ignore
  let incr ~xt a = Xt.incr ~xt (get_self a)
  let decr ~xt a = Xt.decr ~xt (get_self a)

  let rec get ~xt a s i =
    let s = s + Xt.get ~xt (Array.unsafe_get a i) in
    if i = 0 then s else get ~xt a s (i - 1)

  let get ~xt a =
    let i = Array.length a - 1 in
    let s = Xt.get ~xt (Array.unsafe_get a i) in
    if i = 0 then s else get ~xt a s (i - 1)

  let set ~xt a n = add ~xt a (n - get ~xt a)
end

let add a n = if n <> 0 then Loc.fetch_and_add (get_self a) n |> ignore
let incr a = Loc.incr (get_self a)
let decr a = Loc.decr (get_self a)
let get a = Kcas.Xt.commit { tx = Xt.get a }
let set a n = Kcas.Xt.commit { tx = Xt.set a n }
