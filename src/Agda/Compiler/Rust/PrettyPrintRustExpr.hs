module Agda.Compiler.Rust.PrettyPrintRustExpr ( prettyPrintRustExpr, moduleHeader ) where

import Data.List ( intersperse )
import Agda.Compiler.Rust.CommonTypes ( CompiledDef )
import Agda.Compiler.Rust.RustExpr ( RustExpr(..), RustElem(..), FunBody )

prettyPrintRustExpr :: CompiledDef -> String
prettyPrintRustExpr def = case def of
  (TeEnum name fields) ->
    "enum" <> exprSeparator
      <> name
      <> exprSeparator
      <> bracket (
        indent -- TODO this to siplistic indentation
        <> concat (intersperse ", " fields))
  (TeFun fName (RustElem aName aType) resType fBody) ->
      "pub fn" <> exprSeparator
        <> fName
        <> argList (
          aName
          <> typeSeparator <> exprSeparator
          <> aType )
        <> exprSeparator <> funReturnTypeSeparator <> exprSeparator <> resType
        <> exprSeparator <> bracket (
        -- TODO proper indentation for every line of function body
        -- including nested expressions
        indent
        <> (prettyPrintFunctionBody fBody))
        <> defsSeparator
  (TeMod mName defs) ->
    moduleHeader mName
    <> bracket (combineLines (map prettyPrintRustExpr defs))
    <> defsSeparator

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
