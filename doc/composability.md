# About Composability

Composability refers to the ability to combine functions while preserving their properties. For Saturn data structures, the expected properties include atomic consistency (or linearizability) and progress guarantees, such as lock-freedom. Unfortunately, Saturn's data structures are not composable.

Let’s illustrate this with an example. Suppose we want to implement a `split` function for Saturn's queue. The goal is to have multiple domains simultaneously split a source queue into two destination queues based on a predicate. We expect the `split` function to be linearizable, meaning the order of elements in the source queue should be preserved in the destination queues. For instance, `split [0; 1; 2; 3; 4]`, with a predicate that returns `true` for even numbers and `false` otherwise, should produce `[0; 2; 4]` and `[1; 3]`.

Here’s how we can implement `split` using Saturn’s queue functions:

```ocaml
let split source pred true_dest false_dest : bool =
  match Queue.pop source with
  | None -> false
  | Some elt ->
      if pred elt then Queue.push true_dest elt 
      else Queue.push false_dest elt;
      true
```

Domains run the `split` function in parallel until the source queue is empty:

```ocaml
let work source pred true_dest false_dest =
  while split source pred true_dest false_dest do
    ()
  done
```

To test this, we can use the following function:

```ocaml
let test input =
  (* Initialization *)
  let true_dest = Queue.create () in
  let false_dest = Queue.create () in
  let source = Queue.create () in
  List.iter (Queue.push source) input;

  let barrier = Barrier.create 2 in

  (* Predicate: split by parity *)
  let pred elt = elt mod 2 = 0 in

  let d1 =
    Domain.spawn (fun () ->
        Barrier.await barrier;
        work source pred true_dest false_dest)
  in
  let d2 =
    Domain.spawn (fun () ->
        Barrier.await barrier;
        work source pred true_dest false_dest)
  in
  Domain.join d1;
  Domain.join d2;
  (get_content true_dest, get_content false_dest)
```

For an input of `[0; 1; 2; 3; 4]`, the expected output is `([0; 2; 4], [1; 3])`. Most of the time, the function will return the correct result, but occasionally, the queues may appear unsorted.

To measure how often this issue occurs, we can define a `check` function that runs `test` multiple times and counts the number of incorrect results:

```ocaml
let check inputs max_round =
  let expected_even = List.filter (fun elt -> elt mod 2 = 0) inputs in
  let expected_odd = List.filter (fun elt -> elt mod 2 = 1) inputs in
  let rec loop round bugged =
    let even, odd = test inputs in
    if round >= max_round then bugged
    else if even <> expected_even || odd <> expected_odd then
      loop (round + 1) (bugged + 1)
    else loop (round + 1) bugged
  in
  Format.printf "%d/%d rounds are bugged.@." (loop 0 0) max_round
```

Running this function:

```ocaml
# check [0;1;2;3;4;5;6] 1000;;
35/1000 rounds are bugged.
```

As expected, the function is not working correctly. The reason is that our `split` function is not linearizable. While we could make it atomic by introducing a mutex, doing so would sacrifice the progress guarantees of the underlying queue functions, i.e. lock-freedom.

## Extending Data Structures

Note that in the case above, we transfer from and to a queue of the same `int Saturn.Queue.t` type. It may possible to write a `val transfer : t -> t -> unit` function with the right properties and add it directly to `Saturn.Queue` module.

If you think of any such functions, that is useful and missing, let's us know by creating an issue!

## Composable Parallelism-Safe Data Structures

If you need composable parallelism-safe data structures, you can check [kcas_data](https://github.com/ocaml-multicore/cas#programming-with-transactional-data-structures).
