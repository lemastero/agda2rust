{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Agda.Compiler.Rust.AgdaToRustExpr ( compile, compileModule ) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
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
import Agda.Compiler.Rust.RustExpr ( RustExpr(..), RustName, RustType, RustElem(..), FunBody )

compile :: Options -> ModuleEnv -> IsMain -> Definition -> TCM CompiledDef
compile _ _ _ Defn{..}
  = withCurrentModule (qnameModule defName)
  $ getUniqueCompilerPragma "AGDA2RUST" defName >>= \case
      Nothing -> return $ Unhandled "compile" ""
      Just (CompilerPragma _ _) -> 
        return $ compileDefn defName theDef

compileDefn :: QName -> Defn -> CompiledDef
compileDefn defName theDef =
  -- https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-Backend.html#t:Defn
  case theDef of 
    Datatype{dataCons = fields} ->
      compileDataType defName fields
    Function{funCompiled = funDef, funClauses = fc} ->
      compileFunction defName funDef fc
    RecordDefn(RecordData{_recFields = recFields, _recTel = recTel}) ->
      compileRecord defName recFields recTel
    other ->
      Unhandled "compileDefn" (show defName ++ "\n = \n" ++ show theDef)

compileDataType :: QName -> [QName] -> CompiledDef
compileDataType defName fields = ReEnum (showName defName) (map showName fields)

compileRecord :: QName -> [Dom QName] -> Telescope -> CompiledDef
compileRecord defName recFields recTel = ReRec (showName defName) (prettyShow recTel)

compileFunction :: QName
  -> Maybe CompiledClauses
  -> [Clause]
  -> CompiledDef
compileFunction defName funDef fc = ReFun
  (showName defName)
  (RustElem (compileFunctionArgument fc) (compileFunctionArgType fc))
  (compileFunctionResultType fc)
  (compileFunctionBody funDef)

-- TODO this is hacky way to reach find first argument name, assuming function has 1 argument
-- TODO proper way is to handle deBruijn indices
-- TODO see `data Clause` in https://hackage.haskell.org/package/Agda-2.6.4.1/docs/Agda-Syntax-Internal.html
compileFunctionArgument :: [Clause] -> RustName
compileFunctionArgument [] = ""
compileFunctionArgument [fc] = fromDeBruijnPattern (namedThing (unArg (head (namedClausePats fc))))
compileFunctionArgument xs = error "unsupported compileFunctionArgument" ++ (show xs) -- show xs

compileFunctionArgType :: [Clause] -> RustType
compileFunctionArgType [ Clause{clauseTel = ct} ] = fromTelescope ct
compileFunctionArgType xs = error "unsupported compileFunctionArgType" ++ (show xs)

fromTelescope :: Telescope -> RustName
fromTelescope = \case
  ExtendTel a _ -> fromDom a
  other -> error ("unhandled fromType" ++ show other)

fromDom :: Dom Type -> RustName
fromDom x = fromType (unDom x)

compileFunctionResultType :: [Clause] -> RustType
compileFunctionResultType [Clause{clauseType = ct}] = fromMaybeType ct
compileFunctionResultType other = error ("unhandled compileFunctionResultType" ++ show other)

fromMaybeType :: Maybe (Arg Type) -> RustName
fromMaybeType (Just argType) = fromArgType argType
fromMaybeType other = error ("unhandled fromMaybeType" ++ show other)

fromArgType :: Arg Type -> RustName
fromArgType arg = fromType (unArg arg)

fromType :: Type -> RustName
fromType = \case
  a@(El _ ue) -> fromTerm ue
  other -> error ("unhandled fromType" ++ show other)

fromTerm :: Term -> RustName
fromTerm = \case
  Def qname el -> fromQName qname
  other -> error ("unhandled fromTerm" ++ show other)

fromQName :: QName -> RustName
fromQName x = prettyShow (qnameName x)

fromDeBruijnPattern :: DeBruijnPattern -> RustName
fromDeBruijnPattern = \case
    VarP a b -> (dbPatVarName b)
    a@(ConP x y z) -> show a
    other -> error ("unhandled fromDeBruijnPattern" ++ show other)

-- TODO this is wrong for function other than identity
-- see asFriday in Hello.agda vs Hello.rs
compileFunctionBody :: Maybe CompiledClauses -> FunBody
compileFunctionBody (Just funDef) = fromCompiledClauses funDef
compileFunctionBody funDef = error ("unhandled compileFunctionBody " ++ show funDef)

fromCompiledClauses :: CompiledClauses -> FunBody
fromCompiledClauses = \case
  (Done (x:xs) term) -> fromArgName x
  other               -> error ("unhandled fromCompiledClauses " ++ show other)

fromArgName :: Arg ArgName -> FunBody
fromArgName = unArg

showName :: QName -> RustName
showName = prettyShow . qnameName

compileModule :: TopLevelModuleName -> [CompiledDef] -> CompiledDef
compileModule mName cdefs =
  ReMod (moduleName mName) cdefs

moduleName :: TopLevelModuleName -> String
moduleName n = prettyShow (Nel.last (moduleNameParts n))
