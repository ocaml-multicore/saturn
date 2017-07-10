(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type HashDesc = sig
  val load : int;;
  val nb_bucket : int;;
  val hash_function : int -> int;;
end;;

module type S = sig
  type 'a t;;
  (** The type of lock-free hash table. *)

  val to_string : 'a t -> ('a -> string) -> string;;
  (** [to_string h f] returns a string represantation of the hash table [h],
      given a function [f] which gives a string represantation of an element of the list. *)

  val create : unit -> 'a t;;
  (** [create ()] returns a fresh empty hash table. *)

  val find : 'a t -> int -> 'a option;;
  (** [find h k] returns [Some v] if [v] is bounded with [k] in the hash table [h],
      [None] otherwise. *)

  val mem : 'a t -> int -> bool;;
  (** [mem h k] returns [true] if there is some value [v] which is bounded to [k],
      [false] otherwise. *)

  val add : 'a t -> int -> 'a -> unit;;
  (** [add h k v] adds a new binding [(k, v)] to the hash table [h].
      If [k] is already bounded, the binding is replaced. *)

  val remove : 'a t -> int -> bool;;
  (** [remove h k] removes the binding of the key [k] in the hash table [h],
      and returns [true] if it has succeed. *)

  val elem_of : 'a t -> 'a list;;
  (** [elem_of h] returns the list of the value bounded in the hash table [h]. *)
end;;

module Make(Desc : HashDesc) : S;;
