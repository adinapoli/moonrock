module Language.MoonRock.Pretty where

import System.Console.ANSI
import Language.MoonRock.AST
import Text.PrettyPrint.Annotated.Leijen
import Data.Monoid hiding ((<>))


----------------------------------------------------------------------
class Show a => Coloured a where
  colour :: a -> [SGR]


instance Coloured DynExpr  where
  colour (DynBool _ _) = [SetColor Foreground Vivid Cyan]
  colour (DynString _ _) = [SetColor Foreground Vivid Yellow]
  colour (DynNum _ _) = [SetColor Foreground Vivid Green]
  colour (DynSymbol _ _) = [SetColor Foreground Vivid Red]
  colour _ = [SetColor Foreground Vivid White]


----------------------------------------------------------------------
display :: MPretty a => a -> IO ()
display c = do
  setSGR . colour $ c
  putDoc . render $ c
  setSGR []


----------------------------------------------------------------------
class Coloured a => MPretty a where
  render :: a -> Doc a


instance MPretty DynExpr where
  render (DynBool _ v) = text . show $ v
  render (DynString _ v) = text . show $ v
  render (DynSymbol _ v) = text . show $ v
  render (DynNum _ (DInt v)) = text . show $ v
  render (DynNum _ (DDouble v)) = text . show $ v
  render (DynList _ elems) = brackets $ docList (map render elems)
    where
      docList [] = empty
      docList [x] = x
      docList (x : xs) = x <> comma <> docList xs
  render v = text . show $ v
