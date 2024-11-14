(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

(** Lock-free data structures for Multicore OCaml *)

(** {1 Data structures} *)

module Queue = Michael_scott_queue
module Queue_unsafe = Michael_scott_queue_unsafe
module Stack = Treiber_stack
<<<<<<< HEAD
module Bounded_stack = Bounded_stack
=======
module Cue = Cue
>>>>>>> 57f7acb (Bounded queue)
module Work_stealing_deque = Ws_deque
module Single_prod_single_cons_queue = Spsc_queue
module Single_prod_single_cons_queue_unsafe = Spsc_queue_unsafe
module Single_consumer_queue = Mpsc_queue
module Skiplist = Skiplist
module Size = Size
module Htbl = Htbl
module Htbl_unsafe = Htbl_unsafe
