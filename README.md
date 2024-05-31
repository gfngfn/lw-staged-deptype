# Lightweight Dependent Types via Staging

## Abstract

Dependent types are nice to have, particularly for vector/matrix computations; without dependent types such as `Vec n` or `Mat m n`, the inconsistency about the size of vectors/matrices will be detected only as a runtime error. At the same time, however, type-checking using dependent types is usually somewhat tough for users since it will require proofs for judging type equality. Either manual or automated proof is cumbersome for the real-world software development; the manual one easily imposes too many trivial proofs on users and often prevents refactoring, and the automated one is often too time-consuming for CI/CD due to the query for some back-end SMT solvers.

This work attempts to remedy the issue above by using *staging*. Based on the observation that the size of vectors/matrices can be statically determined in a broad sense in many typical cases, we judge such consistency by assertions evaluated as stage-0 (i.e. compile-time) computations. Under this kind of formalization, we can verify the consistency *almost statically* in the sense that inconsistencies as to the size of vectors/matrices will be immediately detected as assertion failures during stage-0 evaluation. In other words, we can do *macro expansion as validation*.


## TODO (theory)

- [x] Assertions corresponding to the equality check of higher-order types
  * We can probably use *Gradual tensor shape checking* \[Hattori, Kobayashi, & Sato 2023\] as a reference
- [x] Error localization for assertion failures
  * This is actually easy because it basically suffices to just add a label `L` to `assert` when type-checking applications `(e_1 e_2)^L` (with a label `L` that indicates a code position) and generating their corresponding `assert`s
- [ ] A surface language that can infer some obvious stage-0 annotations


## TODO (implementation)

- [x] Assertions corresponding to the equality check of higher-order types
- [x] Command-line options
- [x] Handle code positions
- [ ] Add `Mat m n` and operations on it
- [ ] Add realistic examples
