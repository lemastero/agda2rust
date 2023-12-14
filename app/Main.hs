module Main where

import Agda.Compiler.Rust.Backend ( runRustBackend )

main :: IO ()
main = runRustBackend
