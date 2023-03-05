type 'a node_t    = Nil | Node of 'a * bool * 'a node_t Atomic.t * int

type t_metadata = { insertions: int ; deletions: int }

type 'a t =
    { head : 'a node_t Atomic.t; metadata : t_metadata Atomic.t array; prev : 'a node_t Atomic.t }

let default_metadata = Array.init 128 (fun _ -> Atomic.make { insertions=0; deletions=0 })

let create () =
    let dummy = Atomic.make (Node (0, false, (Atomic.make Nil), 0)) in
    { head = dummy; metadata = default_metadata; prev = dummy }

let find s head key =
    let rec aux = fun prev -> begin
        let prev_snapshot = Atomic.get prev in
        match prev_snapshot with
        | Nil -> failwith "impossible case: prev is null"
        | Node (pkey, pmark, curr, ptag) -> begin
            match Atomic.get curr with
            | Nil -> false
            | Node (ckey, cmark, next, ctag) -> begin
                match cmark with
                | false when ckey >= key -> ckey = key
                | false -> begin
                    Atomic.set prev prev_snapshot (Node (cmark, next, ctag));
                    aux prev
                end
                | true -> begin
                    let curr_snapshot = Node (ckey, cmark, next, ctag)
                    match Atomic.compare_and_set prev prev_snapshot curr_snapshot with
                    | false -> aux prev
                    | true -> begin
                        let Atomic.compare_and_set
                    end
                end
            end
        end
    end
