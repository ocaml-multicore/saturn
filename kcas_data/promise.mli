open Kcas

(** A promise of a value to be resolved at some point in the future.

    Example:

    {[
      # let promise, resolver = Promise.create () in
        let domain = Domain.spawn @@ fun () ->
          Printf.printf "Got %d\n%!" (Promise.await promise)
        in
        Promise.resolve resolver 42;
        Domain.join domain
      Got 42
      - : unit = ()
    ]} *)

(** {1 Common interface} *)

type !+'a t
(** The type of a promise of a value of type ['a].  *)

type !-'a u
(** The type of a resolver of a value of type ['a]. *)

type 'a or_exn = ('a, exn) Stdlib.result t
(** The type of a promise of a result of type [('a, exn) result]. *)

val create : unit -> 'a t * 'a u
(** [create ()] returns a new unresolved pair of a promise and a resolver for
    the promise. *)

val create_resolved : 'a -> 'a t
(** [create_resolved x] returns a promise that is already resolved to the given
    value [x]. *)

(** {1 Compositional interface} *)

module Xt :
  Promise_intf.Ops
    with type 'a t := 'a t
    with type 'a or_exn := 'a or_exn
    with type 'a u := 'a u
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
(** Explicit transaction log passing on promises. *)

(** {1 Non-compositional interface} *)

include
  Promise_intf.Ops
    with type 'a t := 'a t
    with type 'a or_exn := 'a or_exn
    with type 'a u := 'a u
    with type ('x, 'fn) fn := 'fn
