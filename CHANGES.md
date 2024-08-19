### 0.5.0

- Optimized Michael-Scott queue with a safe and an unsafe versions (@lyrm,
  @polytypic)
- Optimize spsc queue : unsafe optimizations (@lyrm, @polytypic)
- Optimize spsc queue : optimizations without Obj.magic (@lyrm, @polytypic)
- Tweak treiber stack (@polytypic)
- Disable implicit transitive dependencies (@polytypic)
- Fix dune rules to specify package (@polytypic)
- Lock free skiplist with size (@polytypic, @lyrm, @sooraj-srini)
- Wait-free size for lock free data structures (@polytypic, @lyrm, @nikochiko)

### 0.4.1

- pop_opt, peek, peek_opt functions for queue (@lyrm)
- Remove 'name' field from benchmark results (@Sudha247)
- Better README (@lyrm, @Sudha247, @polytypic, @art-w, @christinerose,
  @ILeandersson, @kayceesrk)
- Add .nojekyll (@lyrm)
- Add a barrier module in tests to replace the use of Semaphore (@lyrm,
  @polytypic)
- Remove .merlin and .ocp-indent files. (@lyrm)
- Correct issue caused by saturn_lockfree module beeing named Lockfree (@lyrm)
- Generate opam files automatically (@sudha247)

## 0.4.0

- Add docs and rename/refactor to add a lockfree package (@lyrm)
- CI clean up and set up Windows CI (@lyrm)
- Adopt OCaml Code of Conduct (@Sudha247)
- Mark alcotest as a test dependency (@Khady)
- Set QCHECK_MSG_INTERVAL to avoid clutter in CI logs (@jmid)
- Fix space leaks in MS Queue (@polytypic, @lyrm)
- Add STM tests for current data structures (@lyrm, @jmid)

## 0.3.1

- Rework dscheck integration to work with utop (@lyrm)
- Add OCaml 4 compatability (@sudha247)
- Add STM ws_deque tests (@jmid, @lyrm)

## 0.3.0

- Add MPSC queue (@lyrm)
- Add SPSC queue (@bartoszmodelski)
- Add MPMC relaxed queue (@bartoszmodelski, @lyrm)
- Add Michael-Scott Queue (@tmcgilchrist, @bartoszmodelski, @lyrm)
- Add Treiber Stack (@tmcgilchrist , @bartoszmodelski, @lyrm)
- Integrate model-checker (DSCheck) (@bartoszmodelski)

## v0.2.0

- Add Chase-Lev Work-stealing deque `Ws_deque`. (@ctk21)
