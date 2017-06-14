(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type S = sig
  type 'a t;;

  val to_string : 'a t -> ('a -> string) -> string;;

  val create : unit -> 'a t;;

  val find : 'a t -> int -> 'a option;;

  val mem : 'a t -> int -> bool;;

  val add : 'a t -> int -> 'a -> unit;;

  val remove : 'a t -> int -> bool;;

  val elem_of : 'a t -> 'a list;;
end;;

module M : S;;
