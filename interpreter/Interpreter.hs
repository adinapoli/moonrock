{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Language.MoonRock.Evaluator
import Language.MoonRock.Parser
import Language.MoonRock.Pretty

import Text.RawString.QQ
import Control.Monad
import System.IO
import qualified Data.Text as T
import qualified Data.List as List
import Text.ParserCombinators.Parsec
import Shelly
import Paths_moonrock


--------------------------------------------------------------------------------
extractCabalVersion :: IO T.Text
extractCabalVersion = do
  cabalManifestDir <- getDataFileName "moonrock.cabal"
  shelly $ silently $ escaping False $ do
    -- Note: This will work only under *nix
    out <- run "cat" [T.pack cabalManifestDir, "|", "grep", "^version:"]
    let ver = T.words out
    return (ver !! 1)


--------------------------------------------------------------------------------
repl :: String -> Int -> IO ()
repl vrn !instr = do
  putStr $ vrn ++ " ~ " ++ show instr ++ " > "
  txt <- getLine
  unless ("quit" `List.isInfixOf` txt) $ do
    case parse rubyFile "" txt of
      Left e -> print e
      Right d -> do
        let evl = eval d
        mapM_ (\e -> pp e >> putStrLn "") evl
    repl vrn (instr + 1)


--------------------------------------------------------------------------------
main :: IO ()
main = do
  vrn <- extractCabalVersion
  putStrLn logo
  putStrLn ""
  hSetBuffering stdout NoBuffering
  repl (T.unpack vrn) 0


--------------------------------------------------------------------------------
logo :: String
logo = [r|
888b     d888                            8888888b.                   888
8888b   d8888                            888   Y88b                  888
88888b.d88888                            888    888                  888
888Y88888P888  .d88b.   .d88b.  88888b.  888   d88P .d88b.   .d8888b 888  888
888 Y888P 888 d88""88b d88""88b 888 "88b 8888888P" d88""88b d88P"    888 .88P
888  Y8P  888 888  888 888  888 888  888 888 T88b  888  888 888      888888K
888   "   888 Y88..88P Y88..88P 888  888 888  T88b Y88..88P Y88b.    888 "88b
888       888  "Y88P"   "Y88P"  888  888 888   T88b "Y88P"   "Y8888P 888  888
|]
