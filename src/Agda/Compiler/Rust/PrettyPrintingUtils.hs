module Agda.Compiler.Rust.PrettyPrintingUtils (
  bracket,
  indent,
  exprSeparator,
  defsSeparator
) where

bracket :: String -> String
bracket str = "{\n" <> str <> "\n}"

indent :: String
indent = "  "

exprSeparator :: String
exprSeparator = " "

defsSeparator :: String
defsSeparator = "\n"
