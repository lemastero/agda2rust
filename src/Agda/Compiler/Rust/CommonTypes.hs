module Agda.Compiler.Rust.CommonTypes (
  Options(..),
  CompiledDef,
  ModuleEnv ) where

import Agda.Compiler.Rust.RustExpr ( RustExpr )

data Options = Options { optOutDir :: Maybe FilePath }

type CompiledDef = RustExpr

type ModuleEnv = ()
