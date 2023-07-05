(** A barrier is a synchronisation tool.

    A barrier of capacity [n] blocks domains until [n] of them are
    waiting. Then these [n] domains can pass. Then the barrier is
    reset.

    Note that this barrier is not starvation-free if there is more
    domains trying to pass it than its capacity.

    This module has been written to help make sure that in `qcheck` tests and
    unitary tests, multiple domains are actually running in parallel.


    If you try this :
    {[
    let example nb_domain =
       let printer i () =
         Format.printf "Domain spawn in %dth position@." i
       in
       let domains = List.init nb_domain (fun i -> Domain.spawn (printer i)) in
       List.iter Domain.join domains
    ]}

    you are most likely going to get the number in order (or almost),
    because printing a line is way much cheaper than spawning a
    domain.

    Whereas with the barrier, you should get a random order :
    {[
    let example_with_barrier nb_domain =
       let barrier = Barrier.create nb_domain in

       let printer i () =
         Barrier.await barrier;
         Format.printf "Domain spawn in %dth position@." i
       in

       let domains = List.init nb_domain (fun i -> Domain.spawn (printer i)) in

       List.iter Domain.join domains
    ]}
*)

type t

val create : int -> t
(** [create c] returns a barrier of capacity [c]. *)

val await : t -> unit
(** A domain calling [await barrier] will only be able to
    progress past this function once the number of domains waiting at
    the barrier is egal to its capacity . *)
