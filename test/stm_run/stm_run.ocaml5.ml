include Intf

let run (type cmd state sut) ?(verbose = true) ?(count = default_count)
    ?(budgetf = default_budgetf) ~name ?make_domain
    (module Spec : STM.Spec
      with type cmd = cmd
       and type state = state
       and type sut = sut) =
  let module Seq = STM_sequential.Make (Spec) in
  let module Dom = struct
    module Spec = Spec
    include STM_domain.Make (Spec)
  end in
  Util.run_with_budget ~budgetf ~count @@ fun count ->
  [
    [ Seq.agree_test ~count ~name:(name ^ " sequential") ];
    (match make_domain with
    | None -> [ Dom.agree_test_par ~count ~name:(name ^ " parallel") ]
    | Some make_domain ->
        make_domain ~count ~name
          (module Dom : STM_domain
            with type Spec.cmd = cmd
             and type Spec.state = state
             and type Spec.sut = sut));
  ]
  |> List.concat
  |> QCheck_base_runner.run_tests ~verbose
