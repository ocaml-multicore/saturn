module type Cue_tests = sig
  include Cue_intf.CUE

  val name : string
end

module Cue : Cue_tests = struct
  include Saturn.Cue

  let name = "htbl_safe"
end

(* module Cue_unsafe : Cue_tests = struct
     include Saturn.Cue_unsafe

     let name = "htbl_unsafe"
   end *)
