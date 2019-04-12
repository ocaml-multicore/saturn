(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type HashDesc = sig
  val load : int;;
  val nb_bucket : int;;
  val hash_function : Domain.id -> int;;
end;;

module type S = sig
  (** The type of lock-free hash table. *)
  type 'a t;;

  (** [to_string h f] returns a string represantation of the hash table [h],
      given a function [f] which gives a string represantation of an element of the list. *)
  val to_string : 'a t -> ('a -> string) -> string;;
  
  (** [create ()] returns a fresh empty hash table. *)
  val create : unit -> 'a t;;

  (** [find h k] returns [Some v] if [v] is bounded with [k] in the hash table [h],
      [None] otherwise. *)
  val find : 'a t -> Domain.id -> 'a option;;

  (** [mem h k] returns [true] if there is some value [v] which is bounded to [k],
      [false] otherwise. *)
  val mem : 'a t -> Domain.id -> bool;;
 
  (** [add h k v] adds a new binding [(k, v)] to the hash table [h].
      If [k] is already bounded, the binding is replaced. *)
  val add : 'a t -> Domain.id -> 'a -> unit;;

  (** [remove h k] removes the binding of the key [k] in the hash table [h],
      and returns [true] if it has succeed. *)
  val remove : 'a t -> Domain.id -> bool;;

  (** [elem_of h] returns the list of the value bounded in the hash table [h]. *)
  val elem_of : 'a t -> 'a list;;
end;;

module Make(Desc : HashDesc) : S;;
