include Intf

let run ~verbose ~count ~name ?make_domain (module Spec : STM.Spec) =
  let module Seq = STM_sequential.Make (Spec) in
  let module Con = STM_thread.Make (Spec) [@alert "-experimental"] in
  [
    [ Seq.agree_test ~count ~name:(name ^ " sequential") ];
    (match make_domain with
    | None -> [ Con.agree_test_conc ~count ~name:(name ^ " concurrent") ]
    | Some _ -> []);
  ]
  |> List.concat
  |> QCheck_base_runner.run_tests ~verbose
