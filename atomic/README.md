# Virtual atomic 

Virtual atomic lets us vary the implementation of `Atomic` module. The are two options: 

* `Stdlib.Atomic` - the standard library implementation. 
* `Dscheck.TracedAtomic` - wrapper provided by a model checker ([DSCheck](https://github.com/ocaml-multicore/dscheck)) for exhaustively testing all interleavings.  

## User perspective 

`Lockfree` defaults to standard library implementation, thus user does not need to do select anything explicitely to use our data structures. 

However, it can be useful to write your own DSCheck tests (e.g. when composing two lock-free structures). In such a case, import `lockfree.atomic_traced` to override the default choice. See [../test/dscheck/dune](../test/dscheck/dune) for example.