module Llist : sig
  type 'a t
  type key = int
  type 'a kind = Dummy | Regular of 'a

  val init : unit -> 'a t
  val add : key -> 'a kind -> 'a t -> bool
  val remove : key -> 'a t -> bool
  val mem : key -> 'a t -> bool
end

module Htbl : sig
  type 'a t
  type key = int

  val init : size_exponent:int -> 'a t
  val add : key -> 'a -> 'a t -> bool
  val find : key -> 'a t -> 'a option
  val mem : key -> 'a t -> bool
  val remove : key -> 'a t -> bool
end

module Htbl_resizable : sig
  type 'a t
  type key = int

  val init : size_exponent:int -> 'a t
  val add : key -> 'a -> 'a t -> bool
  val add_no_resize : int -> 'a -> 'a t -> bool
  val find : key -> 'a t -> 'a option
  val mem : key -> 'a t -> bool
  val remove : key -> 'a t -> bool
  val is_empty : 'a t -> bool
end
