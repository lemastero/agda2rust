# Agda backend for Rust 

## Working with source code

* Starting continuous compilation loop

```sh
ghcid
```

* Build

```sh
cabal build all
```

* Run

The `test/` directory contains an example compilation of `Test.agda` to `Test.rs`
and `Hello.agda` to `Hello.rs`:

```sh
cabal run -- agda2rust --help
cabal run -- agda2rust ./test/Hello.agda
cabal run -- agda2rust ./test/Test.agda
```
* Run tests

```sh
cabal test all
```
