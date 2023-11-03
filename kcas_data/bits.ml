let is_pow_2 n = n land (n - 1) = 0

let max_0 n =
  let m = n asr (Sys.int_size - 1) in
  n land lnot m

let ceil_pow_2_minus_1 n =
  let n = n lor (n lsr 1) in
  let n = n lor (n lsr 2) in
  let n = n lor (n lsr 4) in
  let n = n lor (n lsr 8) in
  let n = n lor (n lsr 16) in
  if Sys.int_size > 32 then n lor (n lsr 32) else n

let ceil_pow_2 n =
  if n <= 1 then 1
  else
    let n = n - 1 in
    let n = ceil_pow_2_minus_1 n in
    n + 1
