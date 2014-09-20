{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Tasty
import           Test.Tasty.Runners
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain allTests


--------------------------------------------------------------------------------
withQuickCheckDepth :: TestName -> Int -> [TestTree] -> TestTree
withQuickCheckDepth tn depth tests =
  localOption (QuickCheckTests depth) (testGroup tn tests)


--------------------------------------------------------------------------------
allTests :: TestTree
allTests = testGroup "parser tests" [
    withQuickCheckDepth "QC properties" 10000 [
      testProperty "parse-pp-roundtrip" testParsePPRoundTrip
    ]
  ]
