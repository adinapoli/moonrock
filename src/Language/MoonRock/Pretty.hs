module Language.MoonRock.Pretty where

import System.Console.ANSI
import Language.MoonRock.AST
import Text.PrettyPrint.Annotated.Leijen


----------------------------------------------------------------------
class Show a => Coloured a where
  colour :: a -> [SGR]


instance Coloured DynExpr  where
  colour (DynBool _ _) = [SetColor Foreground Vivid Cyan] -- FIXME
  colour (DynString _ _) = [SetColor Foreground Vivid Yellow] -- FIXME
  colour (DynNum _ _) = [SetColor Foreground Vivid Green] -- FIXME
  colour (DynSymbol _ _) = [SetColor Foreground Vivid Red] -- FIXME
  colour _ = [Reset]


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
  render v = text . show $ v
