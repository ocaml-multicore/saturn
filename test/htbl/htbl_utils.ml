(* This file enables to define a lower low_buckets value for dscheck testing *)

let[@inline never] impossible () = failwith "impossible"

let ceil_pow_2_minus_1 n =
  let n = Nativeint.of_int n in
  let n = Nativeint.logor n (Nativeint.shift_right_logical n 1) in
  let n = Nativeint.logor n (Nativeint.shift_right_logical n 2) in
  let n = Nativeint.logor n (Nativeint.shift_right_logical n 4) in
  let n = Nativeint.logor n (Nativeint.shift_right_logical n 8) in
  let n = Nativeint.logor n (Nativeint.shift_right_logical n 16) in
  Nativeint.to_int
    (if Sys.int_size > 32 then
       Nativeint.logor n (Nativeint.shift_right_logical n 32)
     else n)

let lo_buckets = 1 lsl 1

and hi_buckets =
  (* floor_pow_2 *)
  let mask = ceil_pow_2_minus_1 Sys.max_array_length in
  mask lxor (mask lsr 1)

let min_buckets_default = 1 lsl 4
and max_buckets_default = Int.min hi_buckets (1 lsl 30 (* Limit of [hash] *))
