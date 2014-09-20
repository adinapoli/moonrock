module Language.MoonRock.Pretty where

import System.Console.ANSI
import Language.MoonRock.AST
import Text.PrettyPrint.Annotated.Leijen
import qualified Data.List as List


cyan :: [SGR]
cyan = [SetColor Foreground Vivid Cyan]

yellow :: [SGR]
yellow = [SetColor Foreground Vivid Yellow]

green :: [SGR]
green = [SetColor Foreground Vivid Green]

red :: [SGR]
red = [SetColor Foreground Vivid Red]

white :: [SGR]
white = [SetColor Foreground Vivid White]

magenta :: [SGR]
magenta = [SetColor Foreground Vivid Magenta]

blue :: [SGR]
blue = [SetUnderlining SingleUnderline, SetColor Foreground Vivid Blue]

----------------------------------------------------------------------
foldDoc :: [Doc a] -> Doc a
foldDoc [] = empty
foldDoc (x : []) = x
foldDoc (x : xs) = x <> foldDoc xs

----------------------------------------------------------------------
nestedBlock :: Int -> [DynExpr] -> Doc [SGR]
nestedBlock offset block = nest offset (foldDoc (map toPretty block))

----------------------------------------------------------------------
docIntersperse :: Doc a -> [Doc a] -> Doc a
docIntersperse _ [] = empty
docIntersperse _ [x] = x
docIntersperse sp (x : xs)  = x <> sp <> docSemi xs

----------------------------------------------------------------------
docSemi :: [Doc a] -> Doc a
docSemi = docIntersperse comma

----------------------------------------------------------------------
docDot :: [Doc a] -> Doc a
docDot = docIntersperse dot

----------------------------------------------------------------------
coloured :: Show a => [SGR] -> a -> Doc [SGR]
coloured colours term = annotate colours (text $ show term)

----------------------------------------------------------------------
class PrettyDynExpr a where
  toPretty :: a -> Doc [SGR]

instance PrettyDynExpr DNum where
  toPretty (DInt v) = coloured green v
  toPretty (DDouble v) = coloured green v

instance PrettyDynExpr Class where
  toPretty (Class str Nothing) = text "class " <> text str
  toPretty (Class str (Just (Class str2 _))) =
    text "class " <> text str <> text " < " <> text str2

instance PrettyDynExpr DOp where
  toPretty (DPlus d1 d2) =
    toPretty d1 <> annotate magenta (text " + ") <> toPretty d2
  toPretty (DSub d1 d2) =
    toPretty d1 <> annotate magenta (text " - ") <> toPretty d2
  toPretty (DMult d1 d2) =
    toPretty d1 <> annotate magenta (text " * ") <> toPretty d2
  toPretty (DNot d1) =
    annotate magenta (text "!") <> toPretty d1
  toPretty (DEqual d1 d2) =
    toPretty d1 <> annotate magenta (text " == ") <> toPretty d2
  toPretty (DNEqual d1 d2) = 
    toPretty d1 <> annotate magenta (text " != ") <> toPretty d2
  toPretty (DIf cond thenBody elseBody) =
    let renderThen = if List.null thenBody
                   then text ""
                   else text "then " <> nestedBlock 4 thenBody
        renderElse = if List.null elseBody
                   then text ""
                   else text "else" <> nestedBlock 4 thenBody
    in text "if " <> toPretty cond
                  <> renderThen
                  <> renderElse
                  <> text "end"
  toPretty (DAnd d1 d2) =
    toPretty d1 <> annotate magenta (text " && ") <> toPretty d2
  toPretty (DOr d1 d2) =
    toPretty d1 <> annotate magenta (text " || ") <> toPretty d2

----------------------------------------------------------------------
instance PrettyDynExpr DynExpr where
  toPretty (DynBool _ v) = coloured cyan v
  toPretty (DynString _ v) = annotate yellow (text v)
  toPretty (DynSymbol _ v) = coloured red v
  toPretty (DynNum _ n) = toPretty n
  toPretty (DynList _ elems) = brackets $ docSemi (map toPretty elems)
  toPretty (DynFunDecl _ fName args body) =
    text "def " <> text fName
                <> parens (docSemi (map toPretty args))
                <> nestedBlock 4 body
                <> text "end"
  toPretty (DynIdentifier _ idf) = annotate blue (text idf)
  toPretty (DynModuleImport _ toImport) =
    text "require " <> annotate yellow (squotes (text toImport))
  toPretty (DynVar _ varId assignment) =
    toPretty varId <> text " = " <> toPretty assignment
  toPretty (DynOp _ op) = toPretty op
  toPretty (DynMethodAccess _ args) = docDot (map toPretty args)
  toPretty (DynClassDecl _ cls body) = toPretty cls <> nestedBlock 4 body


pp :: DynExpr -> IO String
pp de = go (renderCompact . toPretty $ de)
  where
    go v = case v of
      SEmpty -> return ""
      SChar c rest -> (c :) `fmap` go rest
      SAnnotStart a rest -> setSGR a >> go rest
      SAnnotStop rest -> setSGR white >> go rest
      SText _ txt rest -> (txt ++) `fmap` go rest
      SLine ind rest -> (replicate ind ' ' ++) `fmap` go rest
