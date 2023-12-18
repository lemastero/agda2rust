module Main (main) where

import Test.HUnit (
  Test(..)
  , assertEqual
  , failures
  , runTestTT)
import System.Exit ( exitFailure , exitSuccess )
import Agda.Compiler.Rust.Backend ( backend, defaultOptions )
import Agda.Compiler.Rust.PrettyPrintRustExpr ( moduleHeader )

import Agda.Compiler.Backend ( isEnabled )

testIsEnabled :: Test
testIsEnabled = TestCase
  (assertEqual "isEnabled" (isEnabled backend defaultOptions) True)
  
testModuleHeader :: Test
testModuleHeader = TestCase
  (assertEqual "moduleHeader" (moduleHeader "foo") "mod foo ")

tests :: Test
tests = TestList [
    TestLabel "rust Backend is enabled" testIsEnabled,
    TestLabel "produces Rust module header" testModuleHeader
  ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then exitFailure else exitSuccess
