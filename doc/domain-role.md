# Data Structures with Domain Roles

Some provided data structures are designed to work with specific domain configurations. These restrictions optimize their implementation, but failing to respect them may compromise safety properties. These limitations are clearly indicated in the documentation and often reflected in the name of the data structure itself. For instance, a single-consumer queue must have only one domain performing `pop` operations at any given time.

## Example: `Single_prod_single_cons_queue`

As the name suggests, a `Single_prod_single_cons_queue` is designed to be used with exactly one domain performing `push` operations (the producer) and one domain performing `pop` operations (the consumer) at the same time. If multiple domains attempt to `push` (or `pop`) simultaneously, it will break the queue’s safety guarantees and likely lead to unexpected behavior.

Here’s an example of what happens when the queue is misused by giving it an inappropriate alias:

```ocaml
module Queue = Saturn.Single_prod_single_cons_queue
```

In this case, each domain will attempt to `push` 10 times in parallel:

```ocaml
let work id barrier q =
  Barrier.await barrier;
  for i = 0 to 9 do
    Queue.try_push q id |> ignore
  done
```

Our `test` function initializes the queue and creates two domains that simultaneously attempt to `push`:

```ocaml
let test () =
  let q = Queue.create ~size_exponent:5 in
  let barrier = Barrier.create 2 in
  let d1 = Domain.spawn (fun () -> work 1 barrier q) in
  let d2 = Domain.spawn (fun () -> work 2 barrier q) in
  Domain.join d1;
  Domain.join d2;
  q
```

To inspect the contents of the queue after the test, we define a function that extracts all elements into a list:

```ocaml
let get_content q =
  let rec loop acc =
    match Queue.pop_opt q with
    | None -> acc
    | Some a -> loop (a :: acc)
  in
  List.rev (loop [])
```

Let’s run the test:

```ocaml
test () |> get_content;;
- : int list = [2; 1; 1; 1; 1; 1; 1; 1; 1; 1; 2]
```

## Analysis

The resulting queue contains only 11 elements, despite both domains attempting to `push` 10 times each. This happens because the implementation assumes that only one domain will perform `push` operations at any time. Without this assumption, the implementation would need to add synchronization mechanisms, which are intentionally omitted for performance reasons. Consequently, bad interleaving of operations occurs, leading to lost `push`es.


## Conclusion 

This example highlights the importance of adhering to the intended usage of data structures. While these restrictions allow for highly optimized implementations, misusing the data structure—such as having multiple producers or consumers in this case—can lead to unpredictable bugs. Always refer to the documentation and use the appropriate data structure for your concurrency needs to ensure both correctness and performance.
