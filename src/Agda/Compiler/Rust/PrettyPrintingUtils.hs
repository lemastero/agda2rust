module Agda.Compiler.Rust.PrettyPrintingUtils (
  argList,
  bracket,
  indent,
  exprSeparator,
  defsSeparator,
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
