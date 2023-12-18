module Agda.Compiler.Rust.CommonTypes (
  Options(..),
  CompiledDef,
  ModuleEnv ) where

data Options = Options { optOutDir :: Maybe FilePath }

type CompiledDef = String

type ModuleEnv = ()
