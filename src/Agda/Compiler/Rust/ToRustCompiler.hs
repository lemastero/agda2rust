{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Agda.Compiler.Rust.ToRustCompiler ( compile, compileModule, moduleHeader ) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List ( intersperse )
import qualified Data.List.NonEmpty as Nel

import Agda.Compiler.Backend ( IsMain )
import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( Arg(..), ArgName, Named(..), moduleNameParts )
import Agda.Syntax.Internal (
  Clause(..), DeBruijnPattern, DBPatVar(..), Dom(..), unDom, PatternInfo(..), Pattern'(..),
  qnameName, qnameModule, Telescope, Tele(..), Term(..), Type, Type''(..) )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName )
import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
import Agda.TypeChecking.CompiledClause ( CompiledClauses(..), CompiledClauses'(..) )

import Agda.Compiler.Rust.CommonTypes ( Options, CompiledDef, ModuleEnv )
import Agda.Compiler.Rust.PrettyPrintingUtils (
  argList,
  bracket,
  combineLines,
  defsSeparator,
  exprSeparator,
  funReturnTypeSeparator,
  indent,
  typeSeparator )

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
      "Unsupported compileDefn" <> showName defName <> " = " <> prettyShow theDef

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
    <> argList (
      -- TODO handle multiple function clauses and arguments
      compileFunctionArgument fc
      <> typeSeparator <> exprSeparator
      <> compileFunctionArgType fc )
    <> exprSeparator <> funReturnTypeSeparator <> exprSeparator <> compileFunctionResultType fc
    <> exprSeparator <> bracket (
    -- TODO proper indentation for every line of function body
    -- including nested expressions
    -- build intermediate AST and pretty printer for it
    indent
    <> compileFunctionBody funDef)
    <> defsSeparator

-- TODO this is hacky way to reach find first argument name, assuming function has 1 argument
-- TODO proper way is to handle deBruijn indices
-- TODO read docs for `data Clause` section in https://hackage.haskell.org/package/Agda-2.6.4.1/docs/Agda-Syntax-Internal.html
-- TODO start from uncommenting line below and figure out the path to match indices with name and type
-- compileFunctionArgument fc = show fc
compileFunctionArgument :: [Clause] -> CompiledDef
compileFunctionArgument [] = ""
compileFunctionArgument [fc] = fromDeBruijnPattern (namedThing (unArg (head (namedClausePats fc))))
compileFunctionArgument xs = error "unsupported compileFunctionArgument" ++ (show xs)

compileFunctionArgType :: [Clause] -> CompiledDef
compileFunctionArgType [ Clause{clauseTel = ct} ] = fromTelescope ct
compileFunctionArgType xs = error "unsupported compileFunctionArgType" ++ (show xs)

fromTelescope :: Telescope -> CompiledDef
fromTelescope = \case
  ExtendTel a _ -> fromDom a
  other -> error ("unhandled fromType" ++ show other)

fromDom :: Dom Type -> CompiledDef
fromDom x = fromType (unDom x)

compileFunctionResultType :: [Clause] -> CompiledDef
compileFunctionResultType [Clause{clauseType = ct}] = fromMaybeType ct
compileFunctionResultType other = error ("unhandled compileFunctionResultType" ++ show other)

fromMaybeType :: Maybe (Arg Type) -> CompiledDef
fromMaybeType (Just argType) = fromArgType argType
fromMaybeType other = error ("unhandled fromMaybeType" ++ show other)

fromArgType :: Arg Type -> CompiledDef
fromArgType arg = fromType (unArg arg)

fromType :: Type -> CompiledDef
fromType = \case
  a@(El _ ue) -> fromTerm ue
  other -> error ("unhandled fromType" ++ show other)

fromTerm :: Term -> CompiledDef
fromTerm = \case
  Def qname el -> fromQName qname
  other -> error ("unhandled fromTerm" ++ show other)

fromQName :: QName -> CompiledDef
fromQName x = prettyShow (qnameName x)

fromDeBruijnPattern :: DeBruijnPattern -> CompiledDef
fromDeBruijnPattern = \case
    VarP a b -> (dbPatVarName b)
    a@(ConP x y z) -> show a
    other -> error ("unhandled fromDeBruijnPattern" ++ show other)

-- TODO this is wrong for function other than identity
-- see asFriday in Hello.agda vs Hello.rs
compileFunctionBody :: Maybe CompiledClauses -> CompiledDef
compileFunctionBody (Just funDef) = "return" <> exprSeparator <> fromCompiledClauses funDef
compileFunctionBody funDef = error ("unhandled compileFunctionBody " ++ show funDef)

fromCompiledClauses :: CompiledClauses -> CompiledDef
fromCompiledClauses = \case
  (Done (x:xs) term) -> fromArgName x
  other               -> error ("unhandled fromCompiledClauses " ++ show other)

fromArgName :: Arg ArgName -> CompiledDef
fromArgName = unArg

showName :: QName -> CompiledDef
showName = prettyShow . qnameName

compileModule :: TopLevelModuleName -> [CompiledDef] -> String
compileModule mName cdefs =
  moduleHeader (moduleName mName)
  <> bracket (combineLines (map prettyShow cdefs))
  <> defsSeparator

moduleName :: TopLevelModuleName -> String
moduleName n = prettyShow (Nel.head (moduleNameParts n))

moduleHeader :: String -> String
moduleHeader mName = "mod" <> exprSeparator <> mName <> exprSeparator
