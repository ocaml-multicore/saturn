# Motivation: Writing a Concurrent-Safe Stack

The following is a beginner-friendly example to explain why we need data structures specifically designed for multicore programming.

For this exercise, we are trying to define a concurrent-safe (lock-free) stack with the following signature:

```ocaml
module type S = sig 
    type 'a t
    val create : unit -> 'a t
    val push : 'a t -> 'a -> unit
    val pop : 'a t -> 'a option 
end 
``` 

## Sequential Implementation 

We will start with a basic implementation suitable for sequential use and demonstrate why it fails with multiple domains.

```ocaml
module Stack_seq : S = struct
  type 'a t = 'a list ref

  let create () = ref []
  let push st a = st := a :: !st

  let pop st =
    match !st with
    | [] -> None
    | v :: xs ->
        st := xs;
        Some v
end
```

What happens if we try to use this stack with multiple domains?

### Testing Our Implementations  

#### Test Functor
To test our implementation, we need a `test` function that runs the code in parallel. 
Also, we want to be able to inspect the content of our stack.

```ocaml 
module Test (Stack : S) : sig
    val test : unit -> int * 'a list
end  =  struct
    (** Work done by a single domain. The first argument is the domain identifier *)
    let work (id : string) (st : 'a Stack.t) : unit = ...
    
    (** Functions to use to inspect sequentially the content of the stack *)
    let drain (st : 'a Stack.t) : 'a list = ...

    (** The test function returns the number of elements in the stack and its contents *)
    let test () : int * 'a list = ...
end
```

We can define the `drain` function as follows:
```ocaml
 let drain (st : 'a Stack.t) : 'a list =
    let rec loop () =
      match Stack.pop st with None -> [] | Some v -> v :: loop ()
    in
    loop ()
``` 

The `work` function defines what a domain does. For our test, each domain will push its `id` into the stack 10 times.

```ocaml
  let work (id : string) (st : 'a Stack.t) : unit =
    for _ = 0 to 9 do
      Stack.push st id
    done
```

Then let's define our test: it spawns 2 domains that each execute `work` in parallel. `test` returns the content of the stack as well as its length so we can easily see if it contains the 20 elements we expect.

```ocaml
 let test () =
    let st = Stack.create () in
    let domainA = Domain.spawn (fun () -> work "A" st) in
    let domainB = Domain.spawn (fun () -> work "B" st) in
    Domain.join domainA;
    Domain.join domainB;
    let content = drain st in
    (List.length content, content)
```

Our `Test` functor is:
```ocaml 
module Test (Stack : S) : sig
  val test : unit -> int * string list
end = struct
  (** Work done by a single domain. The first argument is the domain identifier *)
  let work (id : string) (st : 'a Stack.t) : unit =
    for _ = 0 to 9 do
      Stack.push st id
    done

  (** Functions to use to inspect sequentially the content of the stack *)
  let drain (st : 'a Stack.t) : 'a list =
    let rec loop () =
      match Stack.pop st with None -> [] | Some v -> v :: loop ()
    in
    loop ()

  (** The test function returns the number of elements in the stack and its contents *)
  let test () : int * 'a list =
    let st = Stack.create () in
    let domainA = Domain.spawn (fun () -> work "A" st) in
    let domainB = Domain.spawn (fun () -> work "B" st) in
    Domain.join domainA;
    Domain.join domainB;
    let content = drain st in
    (List.length content, content)
end
```

#### Testing the `Stack_seq`
Let's try our implementation `Stack_seq`:

```ocaml
# module Test_seq = Test (Stack_seq)
# Test_seq.test ()
- : int * string list =
(20, ["B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "B"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"])
```

Everything seems fine, right? Except, it is not running in parallel, as we can see from the consecutive `B`s (pushed by `domainB`) and `A`s pushed by `domainA`. This is because spawning a domain takes way more time than executing `work`, so `domainA` is long finished before `domainB` is even spawned. One way to overcome this issue is to increase the amount of work done in `work` (for example, by pushing more elements). Another way is to make sure the domains wait for each other before beginning their workload.

#### Adding a Barrier to Ensure Parallelism Happens

We use a basic [barrier implementation](https://github.com/ocaml-multicore/saturn/blob/main/test/barrier/barrier.mli) to do that. Thanks to it, each domain will now wait for the other to reach the barrier before beginning to push.

```ocaml 
module Test (Stack : S) : sig
  val test : unit -> int * string list
end = struct
  (** Work done by a single domain. The first argument is the domain identifier *)
  let work (id : string) (barrier : Barrier.t) (st : 'a Stack.t) : unit =
    Barrier.await barrier;
    for _ = 0 to 9 do
      Stack.push st id
    done

  (** Functions to use to inspect sequentially the content of the stack *)
  let drain (st : 'a Stack.t) : 'a list =
    let rec loop () =
      match Stack.pop st with None -> [] | Some v -> v :: loop ()
    in
    loop ()

  (** The test function returns the number of elements in the stack and its contents *)
  let test () : int * 'a list =
    let st = Stack.create () in
    let barrier = Barrier.create 2 in 
    let domainA = Domain.spawn (fun () -> work "A" barrier st) in
    let domainB = Domain.spawn (fun () -> work "B" barrier st) in
    Domain.join domainA;
    Domain.join domainB;
    let content = drain st in
    (List.length content, content)
end
```

Let's run it again:

```ocaml
# module Test_seq = Test (Stack_seq)
# Test_seq.test ()
- : int * string list =
(11, ["A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "B"; "B"])
```

This is now clearly running in parallel but ... we only have 11 elements in the stack!

### Why It Is Not Working 
Now, the `A`s and the `B`s are interleaved: domains are running in parallel. The resulting stack, however, only contains `11` elements whereas `20` were pushed.

This is because we have a [race condition](https://en.wikipedia.org/wiki/Race_condition) here as `push` is a non-atomic operation. It requires first to read the content of the stack (`!st`) then to write in it (`st := ...`). So, for example, when two domains try to push in parallel into an empty stack the following sequence can happen:
- domain A reads the stack: it is empty
- domain B reads the stack: it is still empty
- domain A pushes `A` on the empty stack it has read before
- domain B pushes `B` on the empty stack it has read before.

This sequence results in a stack containing only one element of value `B`. The element pushed by A is lost because B did not see it.

### Solution: Synchronization Mechanism
This is a very common issue in parallel programming. To prevent it, functions need to be atomically consistent (aka linearizable), meaning they must have a linearization point at which they appear to occur instantly. Such functions can be written with different techniques, including:
- use of [`Atomic`](https://v2.ocaml.org/api/Atomic.html) for mutable variables,
- use of a mutual exclusion mechanism like [`Mutex`](https://v2.ocaml.org/api/Mutex.html).

However, both solutions have their limits. Using mutexes or locks opens the way to deadlock, livelock, priority inversion, etc.; it also often restricts considerably the performance gained by using multiple cores as the parts of the code effectively running in parallel are limited. On the other hand, atomics are - without a complex algorithm to combine them - only a solution for a single shared variable.

## A Concurrent-Safe Implementation 

### First Try
Let's try to replace references by atomics in our code to demonstrate this point:

```ocaml
module Stack_ato : S = struct
  type 'a t = 'a list Atomic.t

  let create () : 'a t = Atomic.make []

  let push (st : 'a t) a =
    let before = Atomic.get st in
    let after = a :: before in
    Atomic.set st after

  let pop st =
    match Atomic.get st with
    | [] -> None
    | v :: xs ->
        Atomic.set st xs;
        Some v
end
```

This implementation of `push` still does a read and write:

```ocaml
# module Test_ato = Test (Stack_ato) 
# Test_ato.test ()
- : int * string list =
(14, ["B"; "B"; "B"; "A"; "A"; "B"; "B"; "A"; "B"; "A"; "A"; "A"; "A"; "B"])
```

and, as expected, it is not working. The interleaving scenario described previously can still happen, meaning our function is not linearizable (or atomically consistent).  

### Second Try

To make it work we actually need a special function: `Atomic.compare_and_set`. 

The `Atomic.compare_and_set` function performs an atomic read-compare-write operation. This means it reads the current value of an atomic variable, compares it to an expected value, and if they match, it writes a new value and returns `true`. Otherwise, it returns `false`. This entire operation is done atomically, ensuring that no other thread can interfere between the read and write steps.

Our new stack module now looks like:
```ocaml
module Stack_lf : S = struct
  type 'a t = 'a list Atomic.t

  let create () : 'a t = Atomic.make []

  let rec push (st : 'a t) a =
    let before = Atomic.get st in
    let after = a :: before in
    if not (Atomic.compare_and_set st before after) then push st a

  let rec pop st =
    let before = Atomic.get st in
    match before with
    | [] -> None
    | v :: after ->
        if Atomic.compare_and_set st before after then Some v else pop st
end
```

```ocaml
# module Test_lf = Test (Stack_lf) 
# Test_lf.test ()
- : int * string list =
(20,
 ["A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "A"; "B"; "B"; "B"; "B"; "B"; "A";
  "B"; "B"; "B"; "B"; "B"])
```

This is finally working as intended!

## Final note 

This final implementation is actually Treiber stack implementation as done in [Saturn](https://ocaml-multicore.github.io/saturn/saturn/Stack/index.html). However, written the way it is here, it will perform poorly. This is one of the main arguments for using `Saturn` data structures instead of doing your own: even a very simple algorithm needs work to perform well.