type 'a t
(** type of fine-grained linked list based on optimistic locking *)

val create : 'a -> 'a t
(** new linked list with dummy head *)

val add : 'a t -> 'a -> bool
(** [list l key] to add a new node to the linkedlist if it already does not exist *)

val remove : 'a t -> 'a -> bool
(** [list l key] to remove a node from the linkedlist if it exists *)

val contains : 'a t -> 'a -> bool
(** [list l key] check if the keys exists in the list *)

val is_empty : 'a t -> bool
(** check if [list] is empty or not *)
