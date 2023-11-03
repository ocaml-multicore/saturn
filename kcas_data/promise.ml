open Kcas

type 'a internal = 'a Magic_option.t Loc.t
type !+'a t
type !-'a u
type 'a or_exn = ('a, exn) Stdlib.result t

external to_promise : 'a internal -> 'a t = "%identity"
external to_resolver : 'a internal -> 'a u = "%identity"
external of_promise : 'a t -> 'a internal = "%identity"
external of_resolver : 'a u -> 'a internal = "%identity"

let create () =
  let p = Loc.make Magic_option.none in
  (to_promise p, to_resolver p)

let create_resolved v = to_promise (Loc.make (Magic_option.some v))

let already_resolved () = invalid_arg "Can't resolve already-resolved promise"
[@@inline never]

module Xt = struct
  let resolve ~xt u v =
    if
      Magic_option.is_some
        (Xt.compare_and_swap ~xt (of_resolver u) Magic_option.none
           (Magic_option.some v))
    then already_resolved ()

  let await ~xt t = Magic_option.get_or_retry (Xt.get ~xt (of_promise t))
  let peek ~xt t = Magic_option.to_option (Xt.get ~xt (of_promise t))
  let is_resolved ~xt t = Magic_option.is_some (Xt.get ~xt (of_promise t))

  let await_exn ~xt t =
    match await ~xt t with Ok value -> value | Error exn -> raise exn

  let resolve_ok ~xt u v = resolve ~xt u (Ok v)
  let resolve_error ~xt u e = resolve ~xt u (Error e)
end

let await t = Loc.get_as Magic_option.get_or_retry (of_promise t)

let resolve u v =
  if
    not
      (Loc.compare_and_set (of_resolver u) Magic_option.none
         (Magic_option.some v))
  then already_resolved ()

let peek t = Magic_option.to_option (Loc.get (of_promise t))
let is_resolved t = Magic_option.is_some (Loc.get (of_promise t))

let await_exn t =
  match await t with Ok value -> value | Error exn -> raise exn

let resolve_ok u v = resolve u (Ok v)
let resolve_error u e = resolve u (Error e)
