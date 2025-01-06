(** Lock-free and resizable hash table.

    The operations provided by this hash table are designed to work as building
    blocks of non-blocking algorithms. Specifically, the operation signatures
    and semantics are designed to allow building
    {{:https://dl.acm.org/doi/10.1145/62546.62593} consensus protocols over
     arbitrary numbers of processes}.

    🏎️ Single key reads with this hash table are actually wait-free rather than
    just lock-free. Internal resizing automatically uses all the threads that
    are trying to write to the hash table. *)

include Htbl_intf.HTBL
