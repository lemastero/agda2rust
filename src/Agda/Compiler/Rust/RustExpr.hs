module Agda.Compiler.Rust.RustExpr (
  RustName,
  RustType,
  RustExpr(..),
  RustElem(..),
  FunBody,
  unHandled
  ) where

type RustName = String
type RustType = String
type FunBody = String

data RustElem = RustElem RustName RustType
  deriving ( Show )

data RustExpr
  = ReMod RustName [RustExpr]
  | ReEnum RustName [RustName]
  | ReFun RustName RustElem RustType FunBody -- TODO [RustElem]
  | ReRec RustName RustName -- TODO [RustElem]
  | Unhandled RustName String
  deriving ( Show )

unHandled :: RustExpr -> Bool
unHandled (Unhandled "" "") = True
unHandled _               = False
