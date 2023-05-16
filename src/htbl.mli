module Llist : sig
  type 'a t
  type key = int
  type 'a kind = Dummy | Regular of 'a

  val create : unit -> 'a t
  val add : key -> 'a kind -> 'a t -> bool
  val replace : key -> 'a kind -> 'a t -> [ `Replaced | `Added ]
  val remove : key -> 'a t -> bool
  val mem : key -> 'a t -> bool
  val find_opt : key -> 'a t -> 'a option
end

module Htbl : sig
  type 'a t
  type key = int

  val create : size_exponent:int -> 'a t
  (** [create ~size_exponent:exp] creates a new, empty map, with
      initial size [2^exp]. *)

  val add : key -> 'a -> 'a t -> bool
  (** [add key data tbl] atomically adds a binding of [key] to [data]
      in table [tbl] if no previous binding exists and returns
      [true]. If a binding of [key] already exists, it returns
      [false]. *)

  val find_opt : key -> 'a t -> 'a option
  (** [find_opt key tbl] atomically returns the current binding of [key] in
      [tbl], or [None] if no such binding exists. This function is
      wait-free. *)

  val mem : key -> 'a t -> bool
  (** [mem key tbl] atomically checks if [key] is bound in [tbl]. This function
      is wait-free.*)

  val remove : key -> 'a t -> bool
  (** [remove key tbl] atomically removes a binding of [key] to [data]
      in table [tbl]. It returns [true] if [key] was bound in [tbl]
      and [false] if it was not. *)

  val replace : key -> 'a -> 'a t -> unit
  (** [replace key data tbl] atomically replaces the current binding
      of [key] in [tbl] by a binding of [key] to [data]. If [key] is
      unbound in [tbl], a binding of [key] to [data] is added to
      [tbl]. *)
end

module Htbl_resizable : sig
  type 'a t
  type key = int

  val create : size_exponent:int -> 'a t
  (** [create ~size_exponent:exp] creates a new, empty map, with
      initial size [2^exp]. *)

  val add : key -> 'a -> 'a t -> bool
  (** [add key data tbl] atomically adds a binding of [key] to [data]
      in table [tbl] if no previous binding exists and returns
      [true]. If a binding of [key] already exists, it returns
      [false]. *)

  val add_no_resize : int -> 'a -> 'a t -> bool
  (** [add_no_resize key data tbl] behaves as [add key data tbl]
      except no resize is performed. *)

  val find_opt : key -> 'a t -> 'a option
  (** [find_opt key tbl] atomically returns the current binding of [key] in
      [tbl], or [None] if no such binding exists. This function is
      wait-free. *)

  val mem : key -> 'a t -> bool
  (** [mem key tbl] atomically checks if [key] is bound in [tbl]. This function
      is wait-free.*)

  val remove : key -> 'a t -> bool
  (** [remove key tbl] atomically removes a binding of [key] to [data]
      in table [tbl]. It returns [true] if [key] was bound in [tbl]
      and [false] if it was not. *)

  val replace : key -> 'a -> 'a t -> unit
  (** [replace key data tbl] atomically replaces the current binding
      of [key] in [tbl] by a binding of [key] to [data]. If [key] is
      unbound in [tbl], a binding of [key] to [data] is added to
      [tbl]. *)

  val is_empty : 'a t -> bool
  (** [is_empty tbl] atomically checks if [tbl] is empty.  *)
end
