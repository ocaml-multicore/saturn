(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type OrderedType = sig
  type t;;
  val compare : t -> t -> int;;
  val hash_t : t -> t;;
  val str : t -> string;;
end;;

module type S = sig
  type key;;
  (** The type of BST key *)

  type 'a t;;
  (** The type of a lockfree Binary Search Tree *)

  val create  : unit -> 'a t;;

  val search : 'a t -> key -> 'a option;;

  val insert : 'a t -> key -> 'a -> unit;;

  val delete : 'a t -> key -> unit;;

  val to_list : 'a t -> (key * key * 'a) list;;

  val heigh : 'a t -> int;;

  val still_bst :'a t -> bool;;
end;;

module Make(Ord : OrderedType) : S with type key = Ord.t;;
