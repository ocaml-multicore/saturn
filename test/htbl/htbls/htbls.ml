module type Htbl_tests = sig
  include Htbl_intf.HTBL

  val name : string
end

module Htbl : Htbl_tests = struct
  include Saturn.Htbl

  let name = "htbl_safe"
end

module Htbl_unsafe : Htbl_tests = struct
  include Saturn.Htbl_unsafe

  let name = "htbl_unsafe"
end
