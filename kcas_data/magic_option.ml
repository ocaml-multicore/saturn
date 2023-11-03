open Kcas

type 'a t = 'a

let none = ref ()
let none = Obj.magic none

external some : 'a -> 'a t = "%identity"

let is_none x = x == none [@@inline]
let is_some x = x != none [@@inline]
let get_or_retry x = if is_none x then Retry.later () else x [@@inline]
let put_or_retry v x = if is_none x then some v else Retry.later () [@@inline]
let take_or_retry x = if is_none x then Retry.later () else none [@@inline]

external get_unsafe : 'a t -> 'a = "%identity"

let to_option x = if is_none x then None else Some x [@@inline]
let of_option = function None -> none | Some x -> some x [@@inline]
