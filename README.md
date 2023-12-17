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

* Compile Rust code

The `test/` directory contains an example compilation of `Test.agda` to `Test.rs`
and `Hello.agda` to `Hello.rs`:

```sh
cabal run -- agda2rust --help
cabal run -- agda2rust test/Hello.agda
cabal run -- agda2rust test/Test.agda
```
* Testing compiled Rust code

```sh
cd test
rustc --crate-type=lib test/Hello.rs
rustc --crate-type=lib test/Test.rs
```

* Run tests

```sh
cabal test all
```
