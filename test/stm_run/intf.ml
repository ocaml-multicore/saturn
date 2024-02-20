module type STM_domain = sig
  module Spec : STM.Spec

  val check_obs :
    (Spec.cmd * STM.res) list ->
    (Spec.cmd * STM.res) list ->
    (Spec.cmd * STM.res) list ->
    Spec.state ->
    bool

  val all_interleavings_ok :
    Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool

  val arb_cmds_triple :
    int ->
    int ->
    (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary

  val arb_triple :
    int ->
    int ->
    (Spec.state -> Spec.cmd QCheck.arbitrary) ->
    (Spec.state -> Spec.cmd QCheck.arbitrary) ->
    (Spec.state -> Spec.cmd QCheck.arbitrary) ->
    (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary

  val arb_triple_asym :
    int ->
    int ->
    (Spec.state -> Spec.cmd QCheck.arbitrary) ->
    (Spec.state -> Spec.cmd QCheck.arbitrary) ->
    (Spec.state -> Spec.cmd QCheck.arbitrary) ->
    (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary

  val interp_sut_res : Spec.sut -> Spec.cmd list -> (Spec.cmd * STM.res) list
  val agree_prop_par : Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool

  val agree_prop_par_asym :
    Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool

  val agree_test_par : count:int -> name:string -> QCheck.Test.t
  val neg_agree_test_par : count:int -> name:string -> QCheck.Test.t
  val agree_test_par_asym : count:int -> name:string -> QCheck.Test.t
  val neg_agree_test_par_asym : count:int -> name:string -> QCheck.Test.t
end
