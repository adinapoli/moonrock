{-# LANGUAGE BangPatterns #-}
module Main where

import Language.MoonRock.Evaluator
import Language.MoonRock.Parser
import Language.MoonRock.Pretty
import Control.Monad
import System.IO

import Text.ParserCombinators.Parsec

repl :: Int -> IO ()
repl !instr = do
  putStr $ ":" ++ show instr ++ " > "
  txt <- getLine
  unless (txt == "quit()") $ do
    case parse rubyFile "" txt of
      Left e -> print e
      Right d -> do
        let evl = eval d
        putStr " => "
        mapM_ display evl
        putStrLn ""
    repl (instr + 1)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  repl 0
