let test n round =
  let open Saturn.Linked_list in
  let try_add sl k = try_add sl k () in

  let rec test count =
    Format.printf "Tour %d@." count;
    if count = 0 then Format.printf "Everything work@."
    else
      let sl = create ~compare () in
      let _ = try_add sl 1 in
      let _ = try_add sl 2 in
      let _ = try_add sl 4 in

      let barrier1 = Atomic.make 0 in
      let barrier2 = Atomic.make 0 in

      let d1 =
        Domain.spawn (fun _ ->
            let rec loop count =
              if count = 0 then true
              else (
                Atomic.incr barrier1;
                while Atomic.get barrier1 <> 2 do
                  ()
                done;

                Atomic.compare_and_set barrier2 2 0 |> ignore;

                if not (try_add sl 3) then false
                else (
                  Atomic.incr barrier2;
                  while Atomic.get barrier2 <> 2 do
                    ()
                  done;
                  Atomic.compare_and_set barrier1 2 0 |> ignore;

                  if remove sl 3 then loop (count - 1) else false))
            in
            loop round)
      in
      let d2 =
        Domain.spawn (fun _ ->
            let rec loop count =
              if count = 0 then true
              else (
                Atomic.incr barrier1;
                while Atomic.get barrier1 <> 2 do
                  ()
                done;

                Atomic.compare_and_set barrier2 2 0 |> ignore;

                if not (remove sl 2) then false
                else (
                  Atomic.incr barrier2;
                  while Atomic.get barrier2 <> 2 do
                    ()
                  done;
                  Atomic.compare_and_set barrier1 2 0 |> ignore;
                  if not (mem sl 2) then
                    let _ = try_add sl 2 in
                    loop (count - 1)
                  else false))
            in
            loop round)
      in
      let r1 = Domain.join d1 in
      Format.printf "D1 joined@.";
      let r2 = Domain.join d2 in
      Format.printf "D2 joined@.";
      if not r1 then Format.printf "R1 bug@.";
      if not r2 then Format.printf "R2 bug@.";

      if r1 && r2 then test (count - 1)
  in
  test n
;;

test 100 100000
