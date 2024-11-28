module Key = struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end

type 'v t = (int, 'v) Htbl.t

let create () = Htbl.create ~hashed_type:(module Key) ()

let rec push t value =
  let key = Int64.to_int (Random.bits64 ()) in
  if not (Htbl.try_add t key value) then push t value

exception Empty

let rec pop_exn t =
  let key = try Htbl.find_random_exn t with Not_found -> raise Empty in
  try Htbl.remove_exn t key with Not_found -> pop_exn t
