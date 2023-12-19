module Agda.Compiler.Rust.PrettyPrintingUtils ( prettyPrintRustExpr, moduleHeader ) where

import Data.List ( intersperse )
import Agda.Compiler.Rust.CommonTypes ( CompiledDef )
import Agda.Compiler.Rust.RustExpr ( RustExpr(..), RustElem(..), FunBody )

prettyPrintRustExpr :: CompiledDef -> String
prettyPrintRustExpr def = case def of
  (ReEnum name fields) ->
    "enum" <> exprSeparator
      <> name
      <> exprSeparator
      <> bracket (
        indent -- TODO this is too simplistic indentation
        <> concat (intersperse ", " fields))
      <> defsSeparator
  (ReFun fName (RustElem aName aType) resType fBody) ->
      "pub fn" <> exprSeparator
        <> fName
        <> argList (
          aName
          <> typeSeparator <> exprSeparator
          <> aType )
        <> exprSeparator <> funReturnTypeSeparator <> exprSeparator <> resType
        <> exprSeparator <> bracket (
        indent <> (prettyPrintFunctionBody fBody))
        <> defsSeparator
  (ReMod mName defs) ->
    moduleHeader mName
    <> bracket (
      defsSeparator -- empty line before first definition in module
      <> combineLines (map prettyPrintRustExpr defs))
    <> defsSeparator 
  (ReRec name payload) -> "pub struct" <> exprSeparator <> name
    <> exprSeparator <> (bracket payload)
    <> defsSeparator
  (Unhandled name payload) -> ""
  -- XXX at the end there should be no Unhandled expression
  -- other -> "unsupported prettyPrintRustExpr " ++ (show other)

bracket :: String -> String
bracket str = "{\n" <> str <> "\n}"

argList :: String -> String
argList str = "(" <> str <> ")"

indent :: String
indent = "  "

exprSeparator :: String
exprSeparator = " "

defsSeparator :: String
defsSeparator = "\n"

typeSeparator :: String
typeSeparator = ":"

funReturnTypeSeparator :: String
funReturnTypeSeparator = "->"

combineLines :: [String] -> String
combineLines xs = unlines (filter (not . null) xs)

prettyPrintFunctionBody :: FunBody -> String
prettyPrintFunctionBody fBody = "return" <> exprSeparator <> fBody <> ";"

moduleHeader :: String -> String
moduleHeader mName = "mod" <> exprSeparator <> mName <> exprSeparator
