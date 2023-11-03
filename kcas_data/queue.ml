open Kcas

type 'a t = {
  front : 'a Elems.t Loc.t;
  middle : 'a Elems.t Loc.t;
  back : 'a Elems.t Loc.t;
}

let alloc ~front ~middle ~back =
  (* We allocate locations in specific order to make most efficient use of the
     splay-tree based transaction log. *)
  let front = Loc.make front
  and middle = Loc.make middle
  and back = Loc.make back in
  { back; middle; front }

let create () = alloc ~front:Elems.empty ~middle:Elems.empty ~back:Elems.empty

let copy q =
  let tx ~xt = (Xt.get ~xt q.front, Xt.get ~xt q.middle, Xt.get ~xt q.back) in
  let front, middle, back = Xt.commit { tx } in
  alloc ~front ~middle ~back

module Xt = struct
  let is_empty ~xt t =
    (* We access locations in order of allocation to make most efficient use of
       the splay-tree based transaction log. *)
    Xt.get ~xt t.front == Elems.empty
    && Xt.get ~xt t.middle == Elems.empty
    && Xt.get ~xt t.back == Elems.empty

  let length ~xt { back; middle; front } =
    Elems.length (Xt.get ~xt front)
    + Elems.length (Xt.get ~xt middle)
    + Elems.length (Xt.get ~xt back)

  let add ~xt x q = Xt.unsafe_modify ~xt q.back @@ Elems.cons x
  let push = add

  (** Cooperative helper to move elems from back to middle. *)
  let back_to_middle ~middle ~back =
    let tx ~xt =
      let xs = Xt.exchange ~xt back Elems.empty in
      if xs == Elems.empty || Xt.exchange ~xt middle xs != Elems.empty then
        raise_notrace Exit
    in
    try Xt.commit { tx } with Exit -> ()

  let take_opt_finish ~xt front elems =
    let elems = Elems.rev elems in
    Xt.set ~xt front (Elems.tl_safe elems);
    Elems.hd_opt elems

  let take_opt ~xt t =
    let front = t.front in
    let elems = Xt.unsafe_update ~xt front Elems.tl_safe in
    if elems != Elems.empty then Elems.hd_opt elems
    else
      let middle = t.middle and back = t.back in
      if not (Xt.is_in_log ~xt middle || Xt.is_in_log ~xt back) then
        back_to_middle ~middle ~back;
      let elems = Xt.exchange ~xt middle Elems.empty in
      if elems != Elems.empty then take_opt_finish ~xt front elems
      else
        let elems = Xt.exchange ~xt back Elems.empty in
        if elems != Elems.empty then take_opt_finish ~xt front elems else None

  let take_blocking ~xt q = Xt.to_blocking ~xt (take_opt q)

  let peek_opt_finish ~xt front elems =
    let elems = Elems.rev elems in
    Xt.set ~xt front elems;
    Elems.hd_opt elems

  let peek_opt ~xt t =
    let front = t.front in
    let elems = Xt.get ~xt front in
    if elems != Elems.empty then Elems.hd_opt elems
    else
      let middle = t.middle and back = t.back in
      if not (Xt.is_in_log ~xt middle || Xt.is_in_log ~xt back) then
        back_to_middle ~middle ~back;
      let elems = Xt.exchange ~xt middle Elems.empty in
      if elems != Elems.empty then peek_opt_finish ~xt front elems
      else
        let elems = Xt.exchange ~xt back Elems.empty in
        if elems != Elems.empty then peek_opt_finish ~xt front elems else None

  let peek_blocking ~xt q = Xt.to_blocking ~xt (peek_opt q)

  let clear ~xt t =
    Xt.set ~xt t.front Elems.empty;
    Xt.set ~xt t.middle Elems.empty;
    Xt.set ~xt t.back Elems.empty

  let swap ~xt q1 q2 =
    let front = Xt.get ~xt q1.front
    and middle = Xt.get ~xt q1.middle
    and back = Xt.get ~xt q1.back in
    let front = Xt.exchange ~xt q2.front front
    and middle = Xt.exchange ~xt q2.middle middle
    and back = Xt.exchange ~xt q2.back back in
    Xt.set ~xt q1.front front;
    Xt.set ~xt q1.middle middle;
    Xt.set ~xt q1.back back

  let seq_of ~front ~middle ~back =
    (* Sequence construction is lazy, so this function is O(1). *)
    Seq.empty
    |> Elems.rev_prepend_to_seq back
    |> Elems.rev_prepend_to_seq middle
    |> Elems.prepend_to_seq front

  let to_seq ~xt t =
    let front = Xt.get ~xt t.front
    and middle = Xt.get ~xt t.middle
    and back = Xt.get ~xt t.back in
    seq_of ~front ~middle ~back

  let take_all ~xt t =
    let front = Xt.exchange ~xt t.front Elems.empty
    and middle = Xt.exchange ~xt t.middle Elems.empty
    and back = Xt.exchange ~xt t.back Elems.empty in
    seq_of ~front ~middle ~back
end

let is_empty q = Kcas.Xt.commit { tx = Xt.is_empty q }
let length q = Kcas.Xt.commit { tx = Xt.length q }

let add x q =
  (* Fenceless is safe as we always update. *)
  Loc.fenceless_modify q.back @@ Elems.cons x

let push = add

let take_opt q =
  (* Fenceless is safe as we revert to a transaction in case we didn't update. *)
  match Loc.fenceless_update q.front Elems.tl_safe |> Elems.hd_opt with
  | None -> Kcas.Xt.commit { tx = Xt.take_opt q }
  | some -> some

let take_blocking q =
  (* Fenceless is safe as we revert to a transaction in case we didn't update. *)
  match Loc.fenceless_update q.front Elems.tl_safe |> Elems.hd_opt with
  | None -> Kcas.Xt.commit { tx = Xt.take_blocking q }
  | Some elem -> elem

let take_all q = Kcas.Xt.commit { tx = Xt.take_all q }

let peek_opt q =
  match Loc.get q.front |> Elems.hd_opt with
  | None -> Kcas.Xt.commit { tx = Xt.peek_opt q }
  | some -> some

let peek_blocking q = Kcas.Xt.commit { tx = Xt.peek_blocking q }
let clear q = Kcas.Xt.commit { tx = Xt.clear q }
let swap q1 q2 = Kcas.Xt.commit { tx = Xt.swap q1 q2 }
let to_seq q = Kcas.Xt.commit { tx = Xt.to_seq q }
let iter f q = Seq.iter f @@ to_seq q
let fold f a q = Seq.fold_left f a @@ to_seq q

exception Empty

let of_option = function None -> raise Empty | Some value -> value [@@inline]
let peek s = peek_opt s |> of_option
let top = peek
let take s = take_opt s |> of_option
