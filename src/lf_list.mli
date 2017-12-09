(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type S = sig
  type 'a t;;
  (** The type of lock-free List *)

  val equal    : 'a t -> 'a t -> bool;;
  (** [equal l1 l2] returns [true] if the list [l1] and [l2] are logically equal. *)

  val to_string: 'a t -> ('a -> string) -> string;;
  (** [to_string l f] returns a string represantation of the list [l],
      given a function [f] which gives a string represantation of an element of the list. *)

  val create   : unit -> 'a t;;
  (** [create ()] returns a fresh empty list. *)

  val cons     : 'a -> 'a t -> unit;;
  (** [cons v l] pushes an element [v] in the front of [l] in none safe way.
      It should not be used if the list [l] should be shared between several threads. *)

  val push     : 'a t -> 'a -> unit;;
  (** [push l v] pushes an element [v] in the front of [l] in a lock-free way. *)

  val pop      : 'a t -> 'a option;;
  (** [pop l] pops an element from the front of [l] in a lock-free way.
      If the list is not empty, it returns [Some v], and [None] otherwise. *)

  val is_empty : 'a t -> bool;;
  (** [is_empty l] returns [true] if the list is empty. *)

(* Ordered list function *)

  val mem      : 'a t -> 'a -> ('a -> 'a -> int) -> bool;;
  (** [mem l v compare] returns [true] if the list [l] contains the value [v].
      The function [compare] gives an ordering on the elements of [l].
      This function should only be called on ordered list (list which has been constructed with [sinsert])*)

  val find     : 'a t -> 'a -> ('a -> 'a -> int) -> 'a option;;
  (** [find l v compare] returns [Some v] if the list [l] contains the value [v], [None] otherwise.
      The function [compare] gives an ordering on the elements of [l].
      This function should only be called on ordered list (list which has been constructed with [sinsert])*)

  val sinsert  : 'a t -> 'a -> ('a -> 'a -> int) -> bool * 'a t;;
  (** [sinsert l v compare] inserts [v] in the list [l] in an ordered way using the ordering function [compare].
      This function should only be called on ordered list (list which has been constructed with [sinsert])*)

  val sdelete  : 'a t -> 'a -> ('a -> 'a -> int) -> bool;;
  (** [sdelete l v compare] returns [true] if the list [l] contains the value [v], and if [v] has been
      succesfully deleted from [l].
      The function [compare] gives an ordering on the elements of [l].
      This function should only be called on ordered list (list which has been constructed with [sinsert])*)

  val elem_of  : 'a t -> 'a list;;
  (** [elem_of l] returns the elements of [l] in a list of the standard library *)
end;;

module M : S;;
