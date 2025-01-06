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

(** Parallelism-safe data structures for Multicore OCaml *)

(** {1 Useful Information}

  {2 Domain-Specific Data Structures}
  Some data structures are optimized for specific domain configurations. These 
  restrictions enhance performance but must be adhered to for maintaining safety 
  properties. These limitations are documented and often reflected in the data 
  structure's name. For example, a single-consumer queue should only have one 
  domain performing `pop` operations at any time.

  For more details, refer to this 
  {{: https://github.com/ocaml-multicore/saturn/blob/main/doc/domain-role.md}{document}}.

  {2 Composability}

  Composability is the ability to combine functions while preserving their 
  properties, such as atomic consistency (linearizability) and progress 
  guarantees (e.g., lock-freedom). Saturn's data structures, however, are not 
  composable.

  For more details, refer to this 
  {{: https://github.com/ocaml-multicore/saturn/blob/main/doc/composability.md}{document}}.
*)

(** {2 Unsafe Data Structures}

    Some data structures have both a normal and an {b unsafe} version. The
    {b unsafe} version uses `Obj.magic`, which can be unsafe, especially with
    flambda2 optimizations.

    The unsafe version is provided to explore performance optimizations that
    require features not currently available in OCaml, such as arrays of atomics
    or atomic fields in records. These versions give an indication of the
    potential performance improvements when such features become available. It
    is recommended to use the normal version unless the performance requirements
    justify the risks associated with the unsafe version. *)

(** {1 Data structures} *)

(** {2 Collections} *)

module Htbl = Htbl
module Htbl_unsafe = Htbl_unsafe
module Skiplist = Skiplist
module Bag = Bag

(** {2 Queues} *)

module Queue = Michael_scott_queue
module Queue_unsafe = Michael_scott_queue_unsafe
module Bounded_queue = Bounded_queue
module Bounded_queue_unsafe = Bounded_queue_unsafe
module Single_consumer_queue = Mpsc_queue
module Single_prod_single_cons_queue = Spsc_queue
module Single_prod_single_cons_queue_unsafe = Spsc_queue_unsafe

(** {2 Stacks} *)

module Stack = Treiber_stack
module Bounded_stack = Bounded_stack

(** {2 Work Stealing Deque}*)

module Work_stealing_deque = Ws_deque

(** {1 Other}*)

module Size = Size
