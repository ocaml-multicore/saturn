open Kcas

type 'a t = 'a Elems.t Loc.t

let create () = Loc.make Elems.empty
let copy s = Loc.make @@ Loc.get s
let of_seq xs = Loc.make (Elems.of_seq_rev xs)

module Xt = struct
  let length ~xt s = Xt.get ~xt s |> Elems.length
  let is_empty ~xt s = Xt.get ~xt s == Elems.empty
  let push ~xt x s = Xt.unsafe_modify ~xt s @@ Elems.cons x
  let pop_opt ~xt s = Xt.unsafe_update ~xt s Elems.tl_safe |> Elems.hd_opt
  let pop_all ~xt s = Elems.to_seq @@ Xt.exchange ~xt s Elems.empty

  let pop_blocking ~xt s =
    Xt.unsafe_update ~xt s Elems.tl_safe |> Elems.hd_or_retry

  let top_opt ~xt s = Xt.get ~xt s |> Elems.hd_opt
  let top_blocking ~xt s = Xt.get ~xt s |> Elems.hd_or_retry
  let clear ~xt s = Xt.set ~xt s Elems.empty
  let swap ~xt s1 s2 = Xt.swap ~xt s1 s2
  let to_seq ~xt s = Elems.to_seq @@ Xt.get ~xt s
end

let length s = Loc.get s |> Elems.length
let is_empty s = Loc.get s == Elems.empty

let push x s =
  (* Fenceless is safe as we always update. *)
  Loc.fenceless_modify s @@ Elems.cons x

let pop_opt s = Loc.update s Elems.tl_safe |> Elems.hd_opt
let pop_all s = Loc.exchange s Elems.empty |> Elems.to_seq

let pop_blocking s =
  (* Fenceless is safe as we always update. *)
  Loc.fenceless_update s Elems.tl_or_retry |> Elems.hd_unsafe

let top_opt s = Loc.get s |> Elems.hd_opt
let top_blocking s = Loc.get_as Elems.hd_or_retry s
let clear s = Loc.set s Elems.empty
let swap s1 s2 = Kcas.Xt.commit { tx = Kcas.Xt.swap s1 s2 }
let to_seq s = Elems.to_seq @@ Loc.get s
let iter f s = Elems.iter f @@ Loc.get s
let fold f a s = Elems.fold f a @@ Loc.get s

exception Empty

let of_option = function None -> raise Empty | Some value -> value [@@inline]
let top s = top_opt s |> of_option
let pop s = pop_opt s |> of_option
