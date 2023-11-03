(** Basically a list where each node includes length, the empty list is a cyclic
    node, and conversions to sequences are performed lazily. *)

type !'a t

val empty : 'a t
val tl_safe : 'a t -> 'a t
val tl_or_retry : 'a t -> 'a t
val length : 'a t -> int
val cons : 'a -> 'a t -> 'a t
val hd_opt : 'a t -> 'a option
val hd_or_retry : 'a t -> 'a
val hd_unsafe : 'a t -> 'a
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val rev : 'a t -> 'a t
val prepend_to_seq : 'a t -> 'a Seq.t -> 'a Seq.t
val to_seq : 'a t -> 'a Seq.t
val of_seq_rev : 'a Seq.t -> 'a t
val rev_prepend_to_seq : 'a t -> 'a Seq.t -> 'a Seq.t
