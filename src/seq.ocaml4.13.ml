include Stdlib.Seq

let rec length_aux accu xs =
  match xs () with Nil -> accu | Cons (_, xs) -> length_aux (accu + 1) xs

let[@inline] length xs = length_aux 0 xs
