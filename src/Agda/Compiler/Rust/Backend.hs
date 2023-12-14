{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Agda.Compiler.Rust.Backend (
  runRustBackend,
  backend,
  defaultOptions,
  moduleHeader ) where

import Data.Maybe ( fromMaybe )
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
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = moduleSetup
  , postModule            = writeModule
  , compileDef            = compile
  , scopeCheckingSuffices = False
  , mayEraseType          = const $ return True
  }

moduleSetup :: Options -> IsMain -> TopLevelModuleName -> Maybe FilePath
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
        return $ handleDef defName theDef

handleDef :: QName
  -> Defn
  -> CompiledDef
handleDef defName theDef =
  case theDef of 
    Datatype{dataCons = fields} ->
      -- TODO get names of fields and separat them with ,
      "enum " <> prettyShow (qnameName defName) <> "{\n" <> prettyShow fields <> "\n}"
    Function{funCompiled = funDef} ->
      -- TODO function arguments
      -- TODO function body
      "fn " <> prettyShow (qnameName defName) <> "() {\n" <> prettyShow funDef <> "\n}"
    _ ->
      prettyShow (qnameName defName) <> " = " <> prettyShow theDef


writeModule :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName -> [CompiledDef]
            -> TCM ModuleRes
writeModule opts _ _ mName cdefs = do
  outDir <- compileDir
  let fileName = moduleNameToFileName mName "rs" 
  liftIO $ putStrLn fileName
  let outFile = fromMaybe outDir (optOutDir opts) <> "/" <> fileName
  unless (all null cdefs) $ liftIO
    $ writeFile outFile
    $ moduleHeader (prettyShow mName) <> unlines cdefs <> moduleFooter

-- TODO figure out how to create TopLevelModuleName
-- TODO and change to TopLevelModuleName -> String
-- TODO ideally test using real file
moduleHeader :: String -> String
moduleHeader mName = "mod " <> mName <> " {\n"

moduleFooter = "\n}\n"