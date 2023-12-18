# Agda backend for Rust

* Compile Agda code to Rust

```sh
cabal run -- agda2rust --help

cabal run -- agda2rust test/hello.agda
cabal run -- agda2rust test/test.agda
```

* Testing compiled Rust code

```sh
rustc --crate-type=lib test/hello.rs
rustc --crate-type=lib test/test.rs
```

# Working with source code

* Starting continuous compilation loop

```sh
ghcid
```

* Build

```sh
cabal build all
```

* Run tests

```sh
cabal test all
```

* CI

Unit tests and compiling sample Agda code to Rust are [run on CI](https://github.com/lemastero/agda2rust/blob/master/.github/workflows/haskell.yml).
