(* The following code is taken from the library [sek] by Arthur Charguéraud
   and François Pottier. *)

(** [blit_circularly_dst a1 i1 a2 i2 k] copies [k] elements from the array
    [a1], starting at index [i1], to the array [a2], starting at index [i2].
    The destination array is regarded as circular, so it is permitted for the
    destination range to wrap around. *)
let blit_circularly_dst a1 i1 a2 i2 k =
  (* The source range must be well-formed. *)
  assert (0 <= k && 0 <= i1 && i1 + k <= Array.length a1);
  (* The destination array must be large enough to hold the data. *)
  let n2 = Array.length a2 in
  assert (k <= n2);
  (* The destination index must be well-formed. *)
  assert (0 <= i2 && i2 < n2);
  (* We need either one or two blits. *)
  if i2 + k <= n2 then Array.blit a1 i1 a2 i2 k
  else
    let k1 = n2 - i2 in
    assert (0 < k1 && k1 < k);
    Array.blit a1 i1 a2 i2 k1;
    Array.blit a1 (i1 + k1) a2 0 (k - k1)

(** [blit_circularly a1 i1 a2 i2 k] copies [k] elements from the array [a1],
    starting at index [i1], to the array [a2], starting at index [i2]. Both
    the source array and the destination array are regarded as circular, so
    it is permitted for the source range or destination range to wrap around.
    [i1] must be comprised between 0 included and [Array.length a1] excluded.
    [i2] must be comprised between 0 included and [Array.length a2] excluded.
    [k] must be comprised between 0 included and [Array.length a2] included. *)
let blit_circularly a1 i1 a2 i2 k =
  let n1 = Array.length a1 in
  (* The source range must be well-formed. *)
  assert (0 <= i1 && i1 < n1);
  assert (0 <= k);
  (* The destination array must be large enough to hold the data. *)
  let n2 = Array.length a2 in
  assert (k <= n2);
  (* The destination index must be well-formed. *)
  assert (0 <= i2 && i2 < n2);
  (* We need either one or two calls to [blit_circularly_dst]. *)
  if i1 + k <= n1 then blit_circularly_dst a1 i1 a2 i2 k
  else
    let k1 = n1 - i1 in
    assert (0 < k1 && k1 < k);
    blit_circularly_dst a1 i1 a2 i2 k1;
    let i2 = i2 + k1 in
    let i2 = if i2 < n2 then i2 else i2 - n2 in
    blit_circularly_dst a1 0 a2 i2 (k - k1)
