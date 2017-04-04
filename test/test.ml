(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 KC Sivaramakrishnan

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

open Printf;;

module Bag = Lf_bag.M;;

let push_test b n init =
  let rec loop i =
    if i < n then begin
      Bag.push b i; loop (i+2)
    end
  in loop init
;;

let rec pop_test b n =
  if n > 0 then
    match Bag.pop b with
    |Some(out) -> print_endline (sprintf "TH%d : %d" (Domain.self ()) out); pop_test b (n-1)
    |None -> print_endline (sprintf "TH%d : Vide" (Domain.self ())); pop_test b (n-1)
;;

let rec push_and_pop b n init =
  let rand1 = Random.int n in
  let rand2 = Random.int n in
  push_test b (n + rand1) init;
  pop_test b (n + rand2);
  push_and_pop b n init
;;

let () =
  let b = Bag.create () in
  let n1 = 10 in
  let n2 = 20 in

  Random.self_init ();

  Domain.spawn (fun () -> push_and_pop b n1 0);
  Domain.spawn (fun () -> push_and_pop b n2 1);

  Unix.sleep 10
;;
