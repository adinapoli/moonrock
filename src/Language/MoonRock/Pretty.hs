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

bold :: [SGR]
bold = [SetUnderlining SingleUnderline]

----------------------------------------------------------------------
foldDoc :: [Doc a] -> Doc a
foldDoc [] = empty
foldDoc (x : []) = x
foldDoc (x : xs) = x <> foldDoc xs

----------------------------------------------------------------------
nestedBlock :: Int -> [DynExpr] -> Doc [SGR]
nestedBlock offset block =
  indent offset (docIntersperse linebreak (map toPretty block))

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
  toPretty (Class str Nothing) = annotate bold (text "class") <+> text str
  toPretty (Class str (Just (Class str2 _))) =
    annotate bold (text "class") <+> text str <> text " < " <> text str2

instance PrettyDynExpr DOp where
  toPretty (DPlus d1 d2) =
    toPretty d1 <+> annotate magenta (text "+") <+> toPretty d2
  toPretty (DSub d1 d2) =
    toPretty d1 <+> annotate magenta (text "-") <+> toPretty d2
  toPretty (DMult d1 d2) =
    toPretty d1 <+> annotate magenta (text "*") <+> toPretty d2
  toPretty (DNot d1) =
    annotate magenta (text "!") <> toPretty d1
  toPretty (DEqual d1 d2) =
    toPretty d1 <+> annotate magenta (text "==") <+> toPretty d2
  toPretty (DNEqual d1 d2) = 
    toPretty d1 <+> annotate magenta (text "!=") <+> toPretty d2
  toPretty (DIf cond thenBody elseBody) =
    let renderThen = if List.null thenBody
                   then text ""
                   else text "then " <> linebreak <> nestedBlock 2 thenBody
        renderElse = if List.null elseBody
                   then text ""
                   else text "else" <> linebreak <> nestedBlock 2 thenBody
    in annotate bold (text "if") <+> toPretty cond
                  <> renderThen
                  <> renderElse
                  <> linebreak
                  <> annotate bold (text "end")
  toPretty (DAnd d1 d2) =
    toPretty d1 <> annotate magenta (text " && ") <> toPretty d2
  toPretty (DOr d1 d2) =
    toPretty d1 <> annotate magenta (text " || ") <> toPretty d2

----------------------------------------------------------------------
instance PrettyDynExpr DynExpr where
  toPretty (DynBool _ v) = coloured cyan v
  toPretty (DynString _ v) = annotate yellow (dquotes (text v))
  toPretty (DynSymbol _ v) = annotate red (text v)
  toPretty (DynNum _ n) = toPretty n
  toPretty (DynNil _ _) = annotate red (text "nil")
  toPretty (DynList _ elems) = brackets $ docSemi (map toPretty elems)
  toPretty (DynFunDecl _ fName args body) =
    annotate bold (text "def") <+> text fName
                <> parens (docSemi (map toPretty args))
                <> linebreak
                <> nestedBlock 2 body
                <> linebreak
                <> annotate bold (text "end")
  toPretty (DynIdentifier _ idf) = annotate blue (text idf)
  toPretty (DynModuleImport _ toImport) =
    text "require" <+> annotate yellow (squotes (text toImport))
  toPretty (DynVar _ varId assignment) =
    toPretty varId <+> text "=" <+> toPretty assignment
  toPretty (DynOp _ op) = toPretty op
  toPretty (DynMethodAccess _ args) = docDot (map toPretty args)
  toPretty (DynClassDecl _ cls body) =
    toPretty cls <> linebreak
                 <> nestedBlock 2 body
                 <> linebreak <> annotate bold (text "end")


pp :: DynExpr -> IO ()
pp de = go (renderPretty 0 0 . toPretty $ de)
  where
    go v = case v of
      SEmpty -> putStr ""
      SChar c rest -> putStr [c] >> go rest
      SAnnotStart a rest -> setSGR a >> go rest
      SAnnotStop rest -> setSGR [] >> go rest
      SText _ txt rest -> putStr txt >> go rest
      SLine ind rest -> do
        putStrLn ""
        putStr (replicate ind ' ')
        go rest


data DynExprIR =
    DynFunDeclIR String [DynExprIR] [DynExprIR]
  | DynNumIR DNum
  | DynStringIR String
  | DynBoolIR Bool
  | DynNilIR ()
  | DynIdentifierIR String
  | DynSymbolIR String
  | DynModuleImportIR String
  | DynVarIR DynExprIR DynExprIR
  | DynListIR [DynExprIR]
  | DynOpIR DOp
  | DynMethodAccessIR [DynExprIR]
  | DynClassDeclIR Class [DynExprIR]
  deriving (Show, Eq)

toIR :: DynExpr -> DynExprIR
toIR (DynFunDecl _ a b c) = DynFunDeclIR a (map toIR b) (map toIR c)
toIR (DynNum _ a) = DynNumIR a
toIR (DynString _ a) = DynStringIR a
toIR (DynBool _ a) = DynBoolIR a
toIR (DynNil _ _) = DynNilIR ()
toIR (DynIdentifier _ a) = DynIdentifierIR a
toIR (DynSymbol _ a) = DynSymbolIR a
toIR (DynModuleImport _ a) = DynModuleImportIR a
toIR (DynVar _ a b) = DynVarIR (toIR a) (toIR b)
toIR (DynList _ a) = DynListIR (map toIR a)
toIR (DynOp _ a) = DynOpIR a
toIR (DynMethodAccess _ a) = DynMethodAccessIR (map toIR a)
toIR (DynClassDecl _ a b) = DynClassDeclIR a (map toIR b)
