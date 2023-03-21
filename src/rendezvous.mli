(** One-shot synchronization between two domains.

When using a lockfree datastructure with a library providing fibers, like
[domainslib] or [eio], the rendezvous should be implemented in terms of the
blocking primitives provided by the scheduler. *)

type 'a t = unit -> ('a -> unit) * (unit -> 'a)
(** The type to create a rendezvous allowing two domains to synchronize
    on a future ['a] value.

    Given a rendezvous [rdv : 'a t], typical usage follows:

    {[
      let send, recv = rdv () in
      Atomic.set release send; (* publish [send] somehere accessible to the other domain *)
      let v = recv () in (* block until [send v] has been called and returns [v] *)
    }]

    The function [send] and [recv] must be called at most once.
*)

val semaphore : 'a t
(** [semaphore] is a rendezvous that uses a semaphore to suspend the domain.
    Recommended when using raw domains without fibers.  *)

val semaphore_unit : unit t
(** Same as [semaphore], specialized for [unit]. *)

val backoff : ?min_wait:int -> ?max_wait:int -> 'a t
(** [backoff] is a rendezvous that locks by spinning, performing a {! Backoff}
    on each failure to make progress.
    Recommended when using raw domains without fibers, if the operation is
    expected to complete soon. *)

val backoff_unit : ?min_wait:int -> ?max_wait:int -> unit t
(** Same as [backoff], specialized for [unit]. *)

val spinlock : 'a t
(** [spinlock] is a rendezvous that locks by spinning, requiring no syscalls.
    Not recommended as it can have terrible performances. *)

val spinlock_unit : unit t
(** Same as [spinlock], specialized for [unit]. *)
