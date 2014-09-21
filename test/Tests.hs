{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests where

import           Test.Tasty.QuickCheck
import           Language.MoonRock.AST
import           Language.MoonRock.Parser
import           Language.MoonRock.Pretty
import           Control.Applicative


--------------------------------------------------------------------------------
testParsePPRoundTrip :: Property
testParsePPRoundTrip =
  forAll arbitrary $ \(dyn :: DynExpr) ->
    (map toIR <$> parseRuby (show $ toPretty dyn)) == Right (map toIR [dyn])
