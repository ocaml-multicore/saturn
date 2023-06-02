type 'a t = {
  array : 'a Option.t Array.t;
  head : int Atomic.t;
  tail : int Atomic.t;
  mask : int;
}

let create ~size_exponent () : 'a t =
  let max_size = 1 lsl size_exponent in
  let array = Array.init max_size (fun _ -> None) in
  let mask = max_size - 1 in
  let head = Atomic.make 0 in
  let tail = Atomic.make 0 in
  { array; head; tail; mask }

let local_push { array; head; tail; mask } item =
  let tail_val = Atomic.get tail in
  let head_val = Atomic.get head in
  let max_size = mask + 1 in

  if tail_val - head_val >= max_size then false
  else
    let index = tail_val land mask in
    Array.set array index (Some item);
    Atomic.set tail (tail_val + 1);
    true

let local_pop { array; head; tail; mask } =
  let head_val = Atomic.get head in
  let tail_val = Atomic.get tail in

  let size = tail_val - head_val in
  assert (size >= 0);
  if size = 0 then None
  else
    (* Local methods should be as fast as possible to get good
       throughput under load. Here, we first increment the head
       and then decide what do. This ensures we never have to
       retry or race with stealers - we've either reserved an item
       or overshot the queue.

       This order works because [local_pop] does not have to
       linearize with [local_push]. Stealers have to take a more
       principled approach. *)
    let reserved = Atomic.fetch_and_add head 1 in
    assert (reserved <= tail_val);
    if reserved = tail_val then (
      (* A steal has succeeded in the meantime, and we've overshot
         the queue. Fix it and return. *)
      Atomic.set head tail_val;
      None)
    else
      let index = reserved land mask in
      let value = Array.get array index in
      assert (Option.is_some value);
      value

let rec steal_one ({ array; head; tail; mask } as t) =
  let head_val = Atomic.get head in
  let tail_val = Atomic.get tail in
  let size = tail_val - head_val in
  assert (size >= -1);
  if size <= 0 then None
  else
    let index = head_val land mask in
    let value = Array.get array index in
    if Atomic.compare_and_set head head_val (head_val + 1) then (
      assert (Option.is_some value);
      value)
    else steal_one t
