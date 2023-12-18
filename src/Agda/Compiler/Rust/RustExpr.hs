module Agda.Compiler.Rust.RustExpr (
  RustName,
  RustType,
  RustExpr(..),
  RustElem(..),
  FunBody
  ) where

type RustName = String
type RustType = String
type FunBody = String

data RustElem = RustElem RustName RustType
  deriving ( Show )

data RustExpr
  = TeMod RustName [RustExpr]
  | TeEnum RustName [RustName]
  | TeFun RustName RustElem RustType FunBody
  | Unhandled RustName String
  deriving ( Show )
