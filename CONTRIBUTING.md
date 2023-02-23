## Contributing

Any contributions are appreciated! Please create issues/PRs to this repo.

### Maintainers

The current list of maintainers is as follows:

- @kayceesrk KC Sivaramakrishnan
- @lyrm Carine Morel
- @Sudha247 Sudha Parimala

### Guidelines for new data structures implementation

Reviewing most implementation takes times. Here are a few guidelines to make it
easier for the reviewers :

- the issue tracker has a good list of data structures to choose from
- implement a well know algorithm (there are a lot !)
  - from a _reviewed_ paper, ideally with proof of main claimed properties (like
    lock-freedom, deadlock freedom etc..)
  - from a well known and used concurrent library (like `java.util.concurrent`)
- write tests with **multiple** domains. All the following tests are expected to
  be provided before a proper review is done, especially for implementations
  that do not come from a well-know algorithm :
  - unitary tests and `qcheck tests` : with one and multiple domains. If domains
    have specific roles (producer, consumer, stealer, etc..), it should appear
    in the tests.
  - tests using `STM` from `multicoretest`
  - (_optional_) `dscheck` tests (for non-blocking implementation)
