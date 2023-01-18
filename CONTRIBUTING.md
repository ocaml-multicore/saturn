## Contributing
Contributions of more lockfree data structures are appreciated! Please create issues/PRs to this repo.

### Maintainers
The current list of maintainers is as follows:

-   @bartoszmodelski Bartosz Modelski
-   @kayceesrk KC Sivaramakrishnan
-   @lyrm Carine Morel
-   @Sudha247 Sudha Parimala

### Guidelines  for new data structures implementation
Reviewing a lock free implementation takes times. Here are a few guidelines to make it easier for the reviewers :
- choose a needed data structure (listed in issues)
- implement a well know algorithm (there are a lot !)
	- from a *reviewed* paper, ideally with proof of lock freedom
	- from a well known and used concurrent library (like `java.util.concurrent`)
-  write tests with **multiple** domains. All the following tests are expected to be provided before a proper review is done, especially for implementation that does not come from a well-know algorithm :
	- unitary tests and `qcheck tests` : with one and multiple domains. If domains have specific roles (producer, consumer, stealer, etc..), it should appear in the tests.
	- tests using `STM` from `multicoretest`
	- (*optional*) `dscheck` tests