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
display :: PrettyDynExpr a => a -> IO ()
display c = do
  setSGR . colour $ c
  putDoc . toPretty $ c
  setSGR []


----------------------------------------------------------------------
class Coloured a => PrettyDynExpr a where
  toPretty :: a -> Doc a


instance PrettyDynExpr DynExpr where
  toPretty (DynBool _ v) = text . show $ v
  toPretty (DynString _ v) = text . show $ v
  toPretty (DynSymbol _ v) = text . show $ v
  toPretty (DynNum _ (DInt v)) = text . show $ v
  toPretty (DynNum _ (DDouble v)) = text . show $ v
  toPretty (DynList _ elems) = brackets $ docList (map toPretty elems)
    where
      docList [] = empty
      docList [x] = x
      docList (x : xs) = x <> comma <> docList xs
  toPretty v = text . show $ v
