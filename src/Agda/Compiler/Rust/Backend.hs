module Agda.Compiler.Rust.Backend (
  runRustBackend,
  backend,
  defaultOptions ) where

import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )
import Data.Maybe ( fromMaybe )
import Data.Version ( showVersion )
import Paths_agda2rust ( version )
import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )

import Agda.Compiler.Common ( curIF, compileDir )
import Agda.Compiler.Backend ( Backend(..), Backend'(..), Recompile(..), IsMain )
import Agda.Interaction.Options ( Flag )
import Agda.Main ( runAgda )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )
import Agda.TypeChecking.Monad (
  TCM,
  TCMT,
  iInsideScope,
  setScope )

import Agda.Compiler.Rust.CommonTypes ( Options(..), CompiledDef, ModuleEnv )
import Agda.Compiler.Rust.AgdaToRustExpr ( compile, compileModule )
import Agda.Compiler.Rust.PrettyPrintingUtils ( prettyPrintRustExpr )

runRustBackend :: IO ()
runRustBackend = runAgda [Backend backend]

instance NFData Options where
  rnf _ = ()

cmdLineFlags :: [OptDescr (Flag Options)]
cmdLineFlags = [
  Option ['o'] ["out-dir"] (ReqArg outdirOpt "DIR")
         "Write output files to DIR. (default: project root)"
  ]

outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts{ optOutDir = Just dir }

defaultOptions :: Options
defaultOptions = Options{ optOutDir = Nothing }

type ModuleRes = ()

backend :: Backend' Options Options ModuleEnv ModuleRes CompiledDef
backend = Backend'
  { backendName           = "agda2rust"
  , backendVersion        = Just (showVersion version)
  , options               = defaultOptions
  , commandLineFlags      = cmdLineFlags
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

writeModule :: Options
  -> ModuleEnv
  -> IsMain
  -> TopLevelModuleName
  -> [CompiledDef]
  -> TCM ModuleRes
writeModule opts _ _ mName cdefs = do
  outDir <- compileDir
  compileLog $ "compiling " <> fileName
  when (null cdefs) $ liftIO
    $ writeFile (outFile outDir)
    $ prettyPrintRustExpr (compileModule mName cdefs)
  where
    fileName = rustFileName mName
    dirName outDir = fromMaybe outDir (optOutDir opts)
    outFile outDir = (dirName outDir) <> "/" <> fileName

rustFileName :: TopLevelModuleName -> FilePath
rustFileName mName = moduleNameToFileName mName "rs" 

compileLog :: String -> TCMT IO ()
compileLog msg = liftIO $ putStrLn msg
