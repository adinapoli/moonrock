{-# LANGUAGE OverloadedStrings #-}
module Language.MoonRock.Parser where


import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec.Language
import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Language.MoonRock.AST
import Data.Maybe


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
            , ">", "<", "=", "!", "~", "==", "!="
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
dynNumber =  try dynInt
         <|> try dynDouble


dynMethodAccess :: Parser DynExpr
dynMethodAccess = do
  spaces
  loc <- getPosition
  callChain <- sepBy dynIdentifier (dot lexer)
  return $ DynMethodAccess loc callChain
 
--
-- DynOp
--

dynNumOp :: String
         -> (DynExpr -> DynExpr -> DOp)
         -> Parser DynExpr
dynNumOp sym fn = do
 spaces
 s1 <- dynNumber
 spaces
 reservedOp lexer sym
 loc <- getPosition
 spaces
 s2 <- dynNumber
 return $ DynOp loc (fn s1 s2)

dynPlus :: Parser DynExpr
dynPlus = dynNumOp "+" DPlus

dynSub :: Parser DynExpr
dynSub = dynNumOp "-" DSub

dynMult :: Parser DynExpr
dynMult = dynNumOp "*" DSub

dynNot :: Parser DynExpr
dynNot = do
 spaces
 reservedOp lexer "!"
 loc <- getPosition
 b1 <- dynBool
 return $ DynOp loc (DNot b1)

dynLogic :: String
         -> (DynExpr -> DynExpr -> DOp)
         -> Parser DynExpr
dynLogic sym fn = do
 spaces
 s1 <- dynBool
 spaces
 reservedOp lexer sym
 loc <- getPosition
 spaces
 s2 <- dynBool
 return $ DynOp loc (fn s1 s2)

dynLogicAnd :: Parser DynExpr
dynLogicAnd = dynLogic "&&" DLogicAnd

dynLogicOr :: Parser DynExpr
dynLogicOr = dynLogic "||" DLogicOr

dynEquality :: String
            -> (DynExpr -> DynExpr -> DOp)
            -> Parser DynExpr
dynEquality sym fn = do
 spaces
 s1 <- dynTerm
 spaces
 reservedOp lexer sym
 loc <- getPosition
 spaces
 s2 <- dynTerm
 return $ DynOp loc (fn s1 s2)

dynEqual :: Parser DynExpr
dynEqual = dynEquality "==" DEqual

dynNEqual :: Parser DynExpr
dynNEqual = dynEquality "!=" DNEqual


dynIf :: Parser DynExpr
dynIf = do
  spaces
  reserved lexer "if"
  loc <- getPosition
  spaces
  pred' <- dynConditional
  spaces
  try $ optional (reserved lexer "then")
  spaces
  ifBody <- many1 dynTerm
  elseB <- optionMaybe $ try $ do
             reserved lexer "else"
             many1 dynTerm
  reserved lexer "end"
  let else' = fromMaybe [] elseB
  return $ DynOp loc (DIf pred' ifBody else')
  
dynConditional :: Parser DynExpr
dynConditional =  try dynBool
              <|> try dynLogicAnd
              <|> try dynLogicOr
              <|> try dynEqual
              <|> try dynNEqual

dynOp :: Parser DynExpr
dynOp =  try dynPlus
     <|> try dynSub
     <|> try dynMult
     <|> try dynNot
     <|> try dynLogicAnd
     <|> try dynLogicOr
     <|> try dynEqual
     <|> try dynNEqual
     <|> try dynIf

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
  expr <-     try dynMethodAccess
          <|> try dynNumber
          <|> try dynIdentifier
          <|> try dynString
          <|> try dynOp
  return $ DynVar loc callChain expr

dynList :: Parser DynExpr
dynList = do
  let bracketsP = brackets lexer
  let commaP = comma lexer
  let valid =  try dynTerm
           <|> try dynOp
           <|> try dynVar
  spaces
  loc <- getPosition
  body <- bracketsP (sepBy valid commaP)
  return $ DynList loc body

dynSymbol :: Parser DynExpr
dynSymbol = do
  spaces
  loc <- getPosition
  syml <- char ':' *> identifier lexer
  return $ DynSymbol loc (':' : syml)

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


dynTerm :: Parser DynExpr
dynTerm =  try dynIdentifier
       <|> try dynNumber
       <|> try dynString
       <|> try dynBool
       <|> try dynSymbol
       <|> try dynList

-- Remember, the lookup order does count
dynExpr :: Parser DynExpr
dynExpr =  try (parens lexer dynExpr)
       <|> dynOp
       <|> dynVar
       <|> dynTerm
       <|> dynModuleImport
       <|> dynFunDecl
       <|> dynClassDecl

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
