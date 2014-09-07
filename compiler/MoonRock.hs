
module Main where

import System.Environment
import System.IO
import Language.MoonRock.Parser
import Text.Parsec.Prim


-- Use optparse applicative.
-- Switch to a streaming parser to not
-- load the entire file into memory.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> withFile fileName ReadMode $ \f -> do
      content <- hGetContents f
      case parse rubyFile "" content of
        Left e -> print e
        Right ast -> print ast
    _ -> print "Usage: mrc <filepath-to-ruby-source>"
