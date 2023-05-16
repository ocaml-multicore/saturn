module Q = Lockfree.Mpmc_queue

module Test = struct
  type t = int Q.t

  let init () = Q.make ~capacity:1 ~dummy:(-1) ()
  let cleanup _ = ()

  open Lin

  let api =
    [
      val_ "push" Q.push (t @-> int @-> returning unit);
      val_ "pop" Q.pop (t @-> returning (option int));
    ]
end

module T = Lin_domain.Make (Test)

let () =
  QCheck_base_runner.run_tests_main
    [ T.lin_test ~count:10_000 ~name:"MPMC Queue" ]
