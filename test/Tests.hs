{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests where

import           Test.Tasty.QuickCheck
import           Language.MoonRock.AST
import           Language.MoonRock.Parser
import           Language.MoonRock.Pretty


--------------------------------------------------------------------------------
testParsePPRoundTrip :: Property
testParsePPRoundTrip =
  forAll arbitrary $ \(dyn :: DynExpr) ->
    parseRuby (show $ toPretty dyn) == Right [dyn]
