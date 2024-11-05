(* Copyright (c) 2023 Vesa Karvonen

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
   REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
   INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
   LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
   OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. *)

module Atomic_array = struct
  type 'a t = 'a Atomic.t array

  let[@inline] at (xs : 'a t) i : 'a Atomic.t = Array.get xs i
  let[@inline] make n v = Array.init n @@ fun _ -> Atomic.make v

  external length : 'a array -> int = "%array_length"

  let unsafe_fenceless_get xs i = Atomic.get xs.(i)

  let[@inline] unsafe_compare_and_set xs i b a =
    Atomic.compare_and_set (at xs i) b a
end
