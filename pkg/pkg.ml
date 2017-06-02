#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "lockfree" @@ fun c ->
  let test x = Pkg.test ~run:false ("test/" ^ x) in
  Ok [ Pkg.mllib "src/lockfree.mllib";
       test "test_list" ;
       test "test_bag" ;
       test "test_bst" ;
       test "test_wsqueue" ;
       test "test_hash"]
