(** Randomized lock-free bag *)

(** {1 API} *)

type !'v t
(** Represents a lock-free bag of elements of type 'v *)

val create : unit -> 'v t
(** [create ()] creates a new empty lock-free bag. *)

val push : 'v t -> 'v -> unit
(** [push bag elt] adds [elt] to the [bag]. *)

exception Empty
(** Raised when {!pop_exn} is applied to an empty bag. *)

val pop_exn : 'v t -> 'v
(** [pop_exn bag] removes and returns a random element of the [bag].

    @raise Empty if the [bag] is empty. *)

val pop_opt : 'v t -> 'v option
(** [pop_opt bag] removes and returns [Some] of a random element of the [bag]
    and [None] if the [bag] is empty. *)

(** {1 Example}

    {[
      # Random.init 0
      - : unit = ()
      # module Bag = Saturn.Bag
      module Bag = Saturn.Bag
      # let t : string Bag.t = Bag.create ()
      val t : string Bag.t = <abstr>

      # let planets = ["Mercury"; "Venus"; "Earth"; "Mars"; "Jupiter"; "Saturn"; "Uranus"; "Neptune"]
      val planets : string list =
        ["Mercury"; "Venus"; "Earth"; "Mars"; "Jupiter"; "Saturn"; "Uranus";
         "Neptune"]
      # List.iter (Bag.push t) planets
      - : unit = ()
      # Bag.pop_exn t
      - : string = "Neptune"
      # Bag.pop_opt t
      - : string option = Some "Saturn"
      # Bag.pop_exn t
      - : string = "Mercury"
      # Bag.pop_exn t
      - : string = "Mars"
      # Bag.pop_exn t
      - : string = "Earth"
      # Bag.pop_exn t
      - : string = "Venus"
      # Bag.pop_exn t
      - : string = "Uranus"
      # Bag.pop_exn t
      - : string = "Jupiter"
      # Bag.pop_exn t
      Exception: Saturn__Bag.Empty.
    ]} *)
