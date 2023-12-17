{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Agda.Compiler.Rust.ToRustCompiler ( compile, compileModule, moduleHeader ) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List ( intersperse )
import qualified Data.List.NonEmpty as Nel

import Agda.Compiler.Backend ( IsMain )
import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Internal ( Clause )
import Agda.Syntax.Internal ( qnameName, qnameModule )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameParts )
import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
import Agda.TypeChecking.CompiledClause ( CompiledClauses )

import Agda.Compiler.Rust.CommonTypes ( Options, CompiledDef, ModuleEnv )
import Agda.Compiler.Rust.PrettyPrintingUtils (
  bracket,
  indent,
  exprSeparator,
  defsSeparator )

compile :: Options -> ModuleEnv -> IsMain -> Definition -> TCM CompiledDef
compile _ _ _ Defn{..}
  = withCurrentModule (qnameModule defName)
  $ getUniqueCompilerPragma "AGDA2RUST" defName >>= \case
      Nothing -> return []
      Just (CompilerPragma _ _) ->
        return $ compileDefn defName theDef

compileDefn :: QName
  -> Defn
  -> CompiledDef
compileDefn defName theDef =
  case theDef of 
    Datatype{dataCons = fields} ->
      compileDataType defName fields
    Function{funCompiled = funDef, funClauses = fc} ->
      compileFunction defName funDef fc
    _ ->
      "UNSUPPORTED " <> showName defName <> " = " <> prettyShow theDef

compileDataType :: QName -> [QName] -> CompiledDef
compileDataType defName fields = "enum" <> exprSeparator
  <> showName defName
  <> exprSeparator
  <> bracket (
    indent
    <> concat (intersperse ", " (map showName fields)))

compileFunction :: QName
  -> Maybe CompiledClauses
  -> [Clause]
  -> CompiledDef
compileFunction defName funDef fc = 
  "pub fn" <> exprSeparator
    <> showName defName
    <> "("
    -- TODO handle multiple function clauses
    <> compileFunctionArgument fc
    <> ")" <> exprSeparator <>
    bracket (
    -- TODO proper indentation for every line of function body
    indent
    <> compileFunctionBody funDef)
    <> defsSeparator

compileFunctionArgument :: [Clause] -> CompiledDef
compileFunctionArgument [] = ""
compileFunctionArgument [fc] = prettyShow fc
compileFunctionArgument xs = error "unsupported compileFunctionArgument" ++ (show xs)

compileFunctionBody :: Maybe CompiledClauses -> CompiledDef
compileFunctionBody funDef = prettyShow funDef

showName :: QName -> CompiledDef
showName = prettyShow . qnameName

compileModule :: TopLevelModuleName -> [CompiledDef] -> String
compileModule mName cdefs =
  moduleHeader (moduleName mName)
  <> bracket (unlines (map prettyShow cdefs))
  <> defsSeparator

moduleName :: TopLevelModuleName -> String
moduleName n = prettyShow (Nel.head (moduleNameParts n))

moduleHeader :: String -> String
moduleHeader mName = "mod" <> exprSeparator <> mName <> exprSeparator
