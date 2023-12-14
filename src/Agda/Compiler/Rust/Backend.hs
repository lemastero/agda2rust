{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Agda.Compiler.Rust.Backend (
  runRustBackend,
  backend,
  defaultOptions,
  moduleHeader ) where

import Data.List ( intersperse )
import Data.Maybe
import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )

import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )

import Data.Version ( showVersion )
import Paths_agda2rust ( version )

import Agda.TypeChecking.Monad
import Agda.Syntax.Abstract.Name ( QName )
import Agda.TypeChecking.Monad.Base ( Defn )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Internal ( qnameName, qnameModule )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )

import Agda.Compiler.Common ( curIF, compileDir )
import Agda.Compiler.Backend ( Backend(..), Backend'(..), Recompile(..), IsMain )

import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
  ( TCM, withCurrentModule, iInsideScope, setScope
  , CompilerPragma(..), getUniqueCompilerPragma )
import Agda.TypeChecking.CompiledClause ( CompiledClauses )
import Agda.Syntax.Internal ( Clause )

import Agda.Main ( runAgda )

runRustBackend :: IO ()
runRustBackend = runAgda [Backend backend]

data Options = Options { optOutDir :: Maybe FilePath }

instance NFData Options where
  rnf _ = ()

outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts{ optOutDir = Just dir }

defaultOptions :: Options
defaultOptions = Options{ optOutDir = Nothing }

type ModuleEnv = ()
type ModuleRes = ()
type CompiledDef = String

backend :: Backend' Options Options ModuleEnv ModuleRes CompiledDef
backend = Backend'
  { backendName           = "agda2rust"
  , backendVersion        = Just (showVersion version)
  , options               = defaultOptions
  , commandLineFlags      =
      [ Option ['o'] ["out-dir"] (ReqArg outdirOpt "DIR")
        "Write output files to DIR. (default: project root)"
      ]
  , isEnabled             = const True
  , preCompile            = return
  , postCompile           = const $ const $ const $ return ()
  , preModule             = moduleSetup
  , postModule            = writeModule
  , compileDef            = compile
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return True
  }

moduleSetup :: Options
  -> IsMain
  -> TopLevelModuleName
  -> Maybe FilePath
  -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup _ _ _ _ = do
  setScope . iInsideScope =<< curIF
  return $ Recompile ()

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
  <>  showName defName
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
    <> compileFunctionArgument (head fc)
    <> ")" <> exprSeparator <>
    bracket (
    -- TODO proper indentation for every line of function body
    indent
    <> compileFunctionBody funDef)
    <> defsSeparator

compileFunctionArgument :: Clause -> CompiledDef
compileFunctionArgument fc = prettyShow fc

compileFunctionBody :: Maybe CompiledClauses -> CompiledDef
compileFunctionBody funDef = prettyShow funDef

showName :: QName -> CompiledDef
showName = prettyShow . qnameName

bracket :: String -> String
bracket str = "{\n" <> str <> "\n}"

writeModule :: Options
  -> ModuleEnv
  -> IsMain
  -> TopLevelModuleName
  -> [CompiledDef]
  -> TCM ModuleRes
writeModule opts _ _ mName cdefs = do
  outDir <- compileDir
  compileLog $ "compiling " <> fileName
  unless (all null cdefs) $ liftIO
    $ writeFile (outFile outDir)
    $ compileModule mName cdefs
  where
    fileName = rustFileName mName
    dirName outDir = fromMaybe outDir (optOutDir opts)
    outFile outDir = (dirName outDir) <> "/" <> fileName

rustFileName :: TopLevelModuleName -> FilePath
rustFileName mName = moduleNameToFileName mName "rs" 

compileModule :: TopLevelModuleName -> [CompiledDef] -> String
compileModule mName cdefs =
  moduleHeader (prettyShow mName)
  <> bracket (unlines (map prettyShow cdefs))
  <> defsSeparator

moduleHeader :: String -> String
moduleHeader mName = "mod" <> exprSeparator <> mName <> exprSeparator

indent :: String
indent = "  "

exprSeparator :: String
exprSeparator = " "

defsSeparator :: String
defsSeparator = "\n"

compileLog :: String -> TCMT IO ()
compileLog msg = liftIO $ putStrLn msg
