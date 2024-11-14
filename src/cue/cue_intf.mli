(* Copyright (c) 2023-2024, Vesa Karvonen <vesa.a.j.k@gmail.com>
   Copyright (c) 2024, Carine Morel <carine.morel.pro@gmail.com>

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

module type CUE = sig
  (** Lock-free bounded queue. *)

  type !'a t
  (** *)

  val create : ?capacity:int -> unit -> 'a t
  (** *)

  val capacity_of : 'a t -> int
  (** *)

  val is_empty : 'a t -> bool
  (** *)

  val length : 'a t -> int
  (** *)

  val peek_opt : 'a t -> 'a option
  (** *)

  val pop_opt : 'a t -> 'a option
  (** *)

  val try_push : 'a t -> 'a -> bool
  (** *)

  (*
val to_list : 'a t -> 'a list
(** *)

val transfer : 'a t -> 'a t
(** *)
*)
end
