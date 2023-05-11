module Llist : sig
  type 'a t
  type key = int
  type 'a kind = Dummy | Regular of 'a

  val init : unit -> 'a t
  val add : key -> 'a kind -> 'a t -> bool
  val replace : key -> 'a kind -> 'a t -> [ `Replaced | `Added ]
  val remove : key -> 'a t -> bool
  val mem : key -> 'a t -> bool
end

module Htbl : sig
  type 'a t
  type key = int

  val init : size_exponent:int -> 'a t
  val add : key -> 'a -> 'a t -> bool
  val replace : key -> 'a -> 'a t -> unit
  val find : key -> 'a t -> 'a option
  val mem : key -> 'a t -> bool
  val remove : key -> 'a t -> bool
end

module Htbl_resizable : sig
  type 'a t
  type key = int

  val init : size_exponent:int -> 'a t
  val add : key -> 'a -> 'a t -> bool
  val replace : key -> 'a -> 'a t -> unit
  val add_no_resize : int -> 'a -> 'a t -> bool
  val find : key -> 'a t -> 'a option
  val mem : key -> 'a t -> bool
  val remove : key -> 'a t -> bool
  val is_empty : 'a t -> bool
end

(*
            let rec find_loop key t prev curr_mkr =
    match curr_mkr with
    | Last | LRemove -> (false, { prev; curr = curr_mkr; next = Last })
    | Normal curr | Remove curr ->
        let rec snip_loop curr_key curr_mrk next_ptr =
          let next_mrk = Atomic.get next_ptr in
          match next_mrk with
          | (Normal _ | Last) when curr_key >= key ->
              (curr_key = key, { prev; curr = curr_mkr; next = next_mrk })
          | Normal _ -> find_loop key t next_ptr next_mrk
          | Last -> (false, { prev = next_ptr; curr = next_mrk; next = Last })
          | Remove next ->
              let new_curr = Normal next in
              if not @@ Atomic.compare_and_set prev curr_mrk new_curr then
                try_again key t
              else
                (*find_loop key t prev (Normal new_curr)*)
                snip_loop curr_key new_curr next.next
          | LRemove ->
              if not @@ Atomic.compare_and_set prev curr_mkr Last then
                try_again key t
              else (false, { prev; curr = next_mrk; next = Last })
        in
        snip_loop curr.key curr_mkr curr.next
        *)
