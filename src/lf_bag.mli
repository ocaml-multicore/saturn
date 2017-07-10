(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type CoreDesc = sig
  val nb_domains : int;;
end;;

module type S = sig
  type 'a t;;
  (** The type of lock-free bag. *)

  val create : unit -> 'a t;;
  (** Create a new bag, which is initially empty. *)

  val is_empty : 'a t -> bool;;
  (** [is_empty b] returns empty if [b] is empty. *)

  val push : 'a t -> 'a -> unit;;
  (** [push b v] pushes [v] into the bag. *)

  val pop : 'a t -> 'a option;;
  (** [pop b] pops an element [e] from the bag and returns
      [Some v] if the bag is non-empty. Otherwise, returns [None]. *)
end;;

module Make(Desc : CoreDesc) : S;;
