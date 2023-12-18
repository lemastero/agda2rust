module Agda.Compiler.Rust.PrettyPrintingUtils (
  argList,
  bracket,
  combineLines,
  defsSeparator,
  exprSeparator,
  funReturnTypeSeparator,
  indent,
  typeSeparator
) where

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
