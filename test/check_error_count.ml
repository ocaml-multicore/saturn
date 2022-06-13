let print_msg_and_exit msg =
  print_endline msg;
  exit 1

let print_usage_and_exit () =
  let msg =
    Printf.sprintf
      "Usage: %s <name-string> <expected-count> <output-filename>" Sys.argv.(0)
  in print_msg_and_exit msg

let check_count s = match int_of_string s with
  | exception Failure _ ->
     print_msg_and_exit (Printf.sprintf "Count not convert %s to an integer" s)
  | i ->
    if i < 0
    then print_msg_and_exit "negative count not allowed"
    else i

let get_error_counts a =
  Array.fold_left (fun (fails,errors,total) line ->
      try
        Scanf.sscanf line "failure (%i tests failed, %i tests errored, ran %i tests)" (fun fl er tl -> (fails+fl,errors+er,total+tl))
      with
      | Scanf.Scan_failure _
      | End_of_file ->
          try
            Scanf.sscanf line "success (ran %i tests)" (fun tl -> (fails,errors,total+tl))
          with
          | Scanf.Scan_failure _
          | End_of_file ->
              (fails,errors,total)) (0,0,0) a

let () =
  if Array.length Sys.argv <> 4 then print_usage_and_exit ();
  let testname, filename = Sys.argv.(1), Sys.argv.(3) in
  let exp_count = check_count Sys.argv.(2) in
  match Arg.read_arg filename with
  | exception Sys_error msg -> print_msg_and_exit msg
  | content ->
     let fails,errors,_total = get_error_counts content in
     let count = fails+errors in
     if count=exp_count
     then Printf.printf "Test '%s' succeeded\n\n" testname
     else
       begin
         Printf.printf "Test '%s' failed: " testname;
         Printf.printf "Expected %i failures but found %i\n\n" exp_count count;
         exit 1
       end
