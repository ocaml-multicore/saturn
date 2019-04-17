(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type CoreDesc = sig
  val nb_domains : int;;
end;;

module type S = sig
  (** The type of lock-free bag. *)
  type 'a t;;

  (** Create a new bag, which is initially empty. *)
  val create : unit -> 'a t;;

  (** [is_empty b] returns empty if [b] is empty. *)
  val is_empty : 'a t -> bool;;

  (** [push b v] pushes [v] into the bag. *)
  val push : 'a t -> 'a -> unit;;

  (** [pop b] pops an element [e] from the bag and returns
      [Some v] if the bag is non-empty. Otherwise, returns [None]. *)
  val pop : 'a t -> 'a option;;
end;;

module Make(Desc : CoreDesc) : S;;
