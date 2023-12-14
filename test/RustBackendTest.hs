module Main (main) where

import Agda.Compiler.Rust.Backend ( backend, defaultOptions )
import Test.HUnit (
  Test(..)
  , assertEqual
  , failures
  , runTestTT)
import System.Exit ( exitFailure , exitSuccess )
import Agda.Compiler.Backend ( isEnabled )

testIsEnabled :: Test
testIsEnabled = TestCase
  (assertEqual "isEnabled" (isEnabled backend defaultOptions) True)

tests :: Test
tests = TestList [TestLabel "rustBackend" testIsEnabled]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then exitFailure else exitSuccess
