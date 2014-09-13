{-# LANGUAGE OverloadedStrings #-}
module Language.MoonRock.Parser where


import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec.Language
import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Language.MoonRock.AST


rubyDef :: LanguageDef s
rubyDef = LanguageDef
            { commentStart   = "=begin"
            , commentEnd     = "=end"
            , commentLine    = "#"
            , nestedComments = True
            , identStart     = letter
            , identLetter    = alphaNum <|> oneOf ['_', '@']
            , opStart        = opLetter emptyDef
            , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
            , reservedOpNames= [
              "+", "-", "^", "/", "%", "&", "|"
            , ">", "<", "=", "!", "~"
            ]
            , reservedNames  = [
                "__LINE__", "__FILE__","__ENCODING__",
                "BEGIN", "END",
                "alias", "and", "begin",
                "break","case","class","def",
                "defined","do","else","elsif",
                "end", "ensure", "false",
                "for","in","module","next",
                "nil","not","or","redo", "require",
                "rescue","retry","return","self",
                "super","then","true","undef",
                "when","yield","if","unless",
                "while", "until"
            ]
            , caseSensitive  = True
            }


lexer :: TokenParser s
lexer = makeTokenParser rubyDef

ws :: Parser ()
ws = whiteSpace lexer

dynModuleImport :: Parser DynExpr
dynModuleImport = do
  loc <- getPosition
  reserved lexer "require"
  module' <- rubyStr
  return $ DynModuleImport loc module'

dynIdentifier :: Parser DynExpr
dynIdentifier = do
  spaces
  loc <- getPosition
  name <- identifier lexer
  spaces
  return $ DynIdentifier loc name

dynFunDecl :: Parser DynExpr
dynFunDecl = do
  let parentsP = parens lexer
  let commaP = commaSep lexer
  spaces
  reserved lexer "def"
  loc <- getPosition
  spaces
  name <- identifier lexer
  args <- parentsP (commaP dynIdentifier)
  body <- sepBy dynExpr (many newline)
  reserved lexer "end"
  return $ DynFunDecl loc name args body


rubyStr :: Parser String
rubyStr = stringLiteral lexer <|>
          between (char '\'')
                  (char '\'')
                  (many1 (noneOf ['\'', '\n']))

dynString :: Parser DynExpr
dynString = getPosition >>= \p ->
  fmap (DynString p) rubyStr


dynInt :: Parser DynExpr
dynInt = getPosition >>= \p ->
  fmap (DynNum p . DInt) (integer lexer)


dynDouble :: Parser DynExpr
dynDouble = getPosition >>= \p ->
  fmap (DynNum p . DDouble) (float lexer)


dynNumber :: Parser DynExpr
dynNumber =  try dynInt <|> try dynDouble


dynMethodAccess :: Parser DynExpr
dynMethodAccess = do
  spaces
  loc <- getPosition
  callChain <- sepBy dynIdentifier (dot lexer)
  return $ DynMethodAccess loc callChain
 
--
-- DynOp
--
dynPlus :: Parser DynExpr
dynPlus = do
 spaces
 s1 <- dynNumber
 spaces
 reservedOp lexer "+"
 loc <- getPosition
 spaces
 s2 <- dynNumber
 return $ DynOp loc (DPlus s1 s2)

dynSub :: Parser DynExpr
dynSub = do
 spaces
 s1 <- dynNumber
 spaces
 reservedOp lexer "-"
 loc <- getPosition
 spaces
 s2 <- dynNumber
 return $ DynOp loc (DSub s1 s2)

dynOp :: Parser DynExpr
dynOp =  try dynPlus
     <|> try dynSub

-- Not sure that the distinction between DynVar
-- and DynMethodAccess is the way to go.
dynVar :: Parser DynExpr
dynVar = do
  spaces
  loc <- getPosition
  callChain <- dynMethodAccess <|> dynIdentifier
  spaces
  reservedOp lexer "="
  spaces
  expr <- dynMethodAccess <|> dynNumber <|> dynIdentifier <|> dynString
  return $ DynVar loc callChain expr


dynSymbol :: Parser DynExpr
dynSymbol = do
  spaces
  loc <- getPosition
  syml <- char ':' *> identifier lexer
  return $ DynSymbol loc syml

dynTrue :: Parser DynExpr
dynTrue = do
  loc <- getPosition
  reserved lexer "true"
  return $ DynBool loc True

dynFalse :: Parser DynExpr
dynFalse = do
  loc <- getPosition
  reserved lexer "false"
  return $ DynBool loc False

dynBool :: Parser DynExpr
dynBool = try dynTrue <|> try dynFalse

classAncestor :: Parser Class
classAncestor = try $ do
  spaces
  reservedOp lexer "<"
  spaces
  cn <- className
  return $ Class cn Nothing

className :: Parser String
className = do
  start <- upper
  rest  <- identifier lexer
  return $ start : rest

dynClassDecl :: Parser DynExpr
dynClassDecl = do
  spaces
  loc <- getPosition
  reserved lexer "class"
  cn <- className
  ancestor <- optionMaybe classAncestor
  body <- sepBy dynExpr (many newline)
  reserved lexer "end"
  let thisClass = Class cn ancestor
  return $ DynClassDecl loc thisClass body

-- Remember, the lookup order does count
dynExpr :: Parser DynExpr
dynExpr =  try dynVar
       <|> try dynOp
       <|> try dynNumber
       <|> try dynString
       <|> try dynBool
       <|> try dynModuleImport
       <|> try dynIdentifier
       <|> try dynFunDecl
       <|> try dynClassDecl

dynHashBang :: Parser ()
dynHashBang = optional $ try $ do
  _ <- string "#!"
  skipMany1 (noneOf "\n")
  _ <- newline
  return ()

rubyFile :: Parser [DynExpr]
rubyFile = do
  dynHashBang
  sepBy dynExpr (many newline)
