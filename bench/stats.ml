let median data =
  let data = List.sort Float.compare data in
  let len = List.length data in
  if len mod 2 == 1 then List.nth data (List.length data / 2)
  else
    let a = List.nth data ((len / 2) - 1) in
    let b = List.nth data (len / 2) in
    (a +. b) /. 2.

let mean data =
  let sum = List.fold_left (fun curr_sum b -> curr_sum +. b) 0. data in
  let n = Int.to_float (List.length data) in
  sum /. n

let stddev data =
  let mean = mean data in
  let sum =
    List.fold_left
      (fun curr_sum datapoint ->
        let squared_diff = Float.pow (datapoint -. mean) 2. in
        curr_sum +. squared_diff)
      0. data
  in
  let n = Int.to_float (List.length data) in
  Float.sqrt (sum /. n)

let cmp ?(epsilon = 0.01) a b = Float.abs (a -. b) < epsilon

let sanity_checks () =
  assert (cmp (mean [ 1.; 5. ]) 3.);
  assert (cmp (mean [ 1.; 3.; 7. ]) 3.6666);
  assert (cmp (stddev [ 1.; 5. ]) 2.);
  assert (cmp (stddev [ 1.; 3.; 7. ]) 2.4944)
;;

sanity_checks ()
