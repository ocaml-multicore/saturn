#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "lockfree" @@ fun c ->
  Ok [ Pkg.mllib "src/lockfree.mllib";
       Pkg.test "test/test"; ]
