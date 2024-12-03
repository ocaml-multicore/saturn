module Key = struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end

type 'v t = (int, 'v) Htbl.t

let create () = Htbl.create ~hashed_type:(module Key) ()

(* *)

let rec push t value =
  let key = Int64.to_int (Random.bits64 ()) in
  if not (Htbl.try_add t key value) then push t value

(* *)

exception Empty

type ('a, _) poly = Option : ('a, 'a option) poly | Value : ('a, 'a) poly

let rec pop_as : type a r. a t -> (a, r) poly -> r =
 fun t poly ->
  match Htbl.find_random_exn t with
  | key -> begin
      match Htbl.remove_exn t key with
      | value -> ( match poly with Option -> Some value | Value -> value)
      | exception Not_found -> pop_as t poly
    end
  | exception Not_found -> (
      match poly with Option -> None | Value -> raise Empty)

let pop_exn t = pop_as t Value
let pop_opt t = pop_as t Option
