(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type S = sig
  type 'a t;;

  val equal    : 'a t -> 'a t -> bool;;

  val print    : 'a t -> ('a -> unit) -> unit;;

  val to_string: 'a t -> ('a -> string) -> string;;

  val create   : unit -> 'a t;;

  val cons     : 'a -> 'a t -> unit;;

  val push     : 'a t -> 'a -> unit;;

  val pop      : 'a t -> 'a option;;

  val is_empty : 'a t -> bool;;

  val mem      : 'a t -> 'a -> ('a -> 'a -> int) -> bool;;

  val find     : 'a t -> 'a -> ('a -> 'a -> int) -> 'a option;;

  val sinsert  : 'a t -> 'a -> ('a -> 'a -> int) -> 'a t;;

  val sdelete  : 'a t -> 'a -> ('a -> 'a -> int) -> bool;;
end;;

module M : S;;
