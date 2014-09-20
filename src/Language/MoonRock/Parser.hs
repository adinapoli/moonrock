{-# LANGUAGE OverloadedStrings #-}
module Language.MoonRock.Parser where


import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec.Language
import Text.Parsec.Expr
import Control.Monad.Identity
import Text.Parsec.Token
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec
import Language.MoonRock.AST
import Data.Maybe
import Data.Char


getLoc :: Monad m => ParsecT s u m Loc
getLoc = do
  pos <- getPosition
  let ln = sourceLine pos
  let cl = sourceColumn pos
  return (ln, cl)


rubyDef :: LanguageDef s
rubyDef = emptyDef
            { commentStart   = "=begin"
            , commentEnd     = "=end"
            , commentLine    = "#"
            , nestedComments = True
            , identStart     = lower <|> oneOf ['_', '@']
            , identLetter    = alphaNum <|> oneOf ['_', '@']
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


-- opTable = [
--     [
--       "+", "-", "^", "/", "%", "&", "|"
--     , ">", "<", "=", "!", "~", "==", "!="
--     ]
--   ]

----------------------------------------------------------------------
operatorTable :: [[Operator String () Identity DynExpr]]
operatorTable = [ [ prefixOp "!" DNot ]
                , [ binaryOp "&&" DAnd AssocLeft
                  , binaryOp "||" DOr AssocLeft
                  ]
                , [ binaryOp "*" DMult AssocLeft ]
                , [ binaryOp "+" DPlus AssocLeft
                  , binaryOp "-" DSub AssocLeft
                  ]
                , [ binaryOp "==" DEqual AssocNone
                  , binaryOp "!=" DNEqual AssocNone]
                ]

----------------------------------------------------------------------
prefixOp :: String -> (DynExpr -> DOp)
         -> Operator String () Identity DynExpr
prefixOp n f = Prefix $ do
  spaces *> reservedOp lexer n <* spaces
  loc <- getLoc
  return $ \a -> DynOp loc (f a)

----------------------------------------------------------------------
binaryOp :: String -> (DynExpr -> DynExpr -> DOp)
         -> Assoc -> Operator String () Identity DynExpr
binaryOp n f = Infix $ do
   spaces *> reservedOp lexer n <* spaces
   loc <- getLoc
   return $ \a b -> DynOp loc (f a b)

----------------------------------------------------------------------
opTerm :: Parser DynExpr
opTerm = do
  buildExpressionParser operatorTable psr
  where
    psr = dynTerm

----------------------------------------------------------------------
lexer :: TokenParser s
lexer = makeTokenParser rubyDef

----------------------------------------------------------------------
ws :: Parser ()
ws = whiteSpace lexer

----------------------------------------------------------------------
dynModuleImport :: Parser DynExpr
dynModuleImport = do
  loc <- getLoc
  reserved lexer "require"
  module' <- rubyStr
  return $ DynModuleImport loc module'

----------------------------------------------------------------------
dynIdentifier :: Parser DynExpr
dynIdentifier = do
  spaces
  loc <- getLoc
  name <- identifier lexer
  spaces
  return $ DynIdentifier loc name

----------------------------------------------------------------------
dynFunDecl :: Parser DynExpr
dynFunDecl = do
  let parentsP = parens lexer
  let commaP = commaSep lexer
  let semiP = semi lexer
  spaces
  reserved lexer "def"
  loc <- getLoc
  spaces
  name <- identifier lexer
  args <- parentsP (commaP dynIdentifier)
  body <- sepBy dynExpr (semiP <|> many newline)
  reserved lexer "end"
  return $ DynFunDecl loc name args body


----------------------------------------------------------------------
rubyStr :: Parser String
rubyStr = stringLiteral lexer <|>
          between (char '\'')
                  (char '\'')
                  (many1 (noneOf ['\'', '\n']))

----------------------------------------------------------------------
dynString :: Parser DynExpr
dynString = getLoc >>= \p ->
  fmap (DynString p) rubyStr


----------------------------------------------------------------------
dynInt :: Parser DynExpr
dynInt = getLoc >>= \p ->
  fmap (DynNum p . DInt) (integer lexer)


----------------------------------------------------------------------
dynDouble :: Parser DynExpr
dynDouble = getLoc >>= \p ->
  fmap (DynNum p . DDouble) (float lexer)


----------------------------------------------------------------------
dynNumber :: Parser DynExpr
dynNumber =  dynInt
         <|> dynDouble


----------------------------------------------------------------------
dynMethodAccess :: Parser DynExpr
dynMethodAccess = try $ do
  spaces
  loc <- getLoc
  firstId <- dynIdentifier
  skipMany1 (string ".")
  callChain <- sepBy dynIdentifier (dot lexer)
  return $ DynMethodAccess loc (firstId : callChain)

----------------------------------------------------------------------
dynIf :: Parser DynExpr
dynIf = do
  spaces
  reserved lexer "if"
  loc <- getLoc
  spaces
  pred' <- dynBoolLike
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


dynBoolLike :: Parser DynExpr
dynBoolLike = opTerm <|> dynBool


-- Not sure that the distinction between DynVar
-- and DynMethodAccess is the way to go.
dynVar :: Parser DynExpr
dynVar = try $ do
  spaces
  loc <- getLoc
  callChain <- dynMethodAccess <|> dynIdentifier
  spaces
  reservedOp lexer "="
  spaces
  expr <-     dynMethodAccess
          <|> dynNumber
          <|> dynIdentifier
          <|> dynString
          <|> dynBoolLike
  return $ DynVar loc callChain expr

dynList :: Parser DynExpr
dynList = do
  let bracketsP = brackets lexer
  let commaP = comma lexer
  let valid =  dynTerm
           <|> dynBoolLike
           <|> dynVar
  spaces
  loc <- getLoc
  body <- bracketsP (sepBy valid commaP)
  return $ DynList loc body

dynSymbol :: Parser DynExpr
dynSymbol = do
  spaces
  loc <- getLoc
  syml <- char ':' *> identifier lexer
  return $ DynSymbol loc (':' : syml)

dynTrue :: Parser DynExpr
dynTrue = do
  loc <- getLoc
  reserved lexer "true"
  return $ DynBool loc True

dynFalse :: Parser DynExpr
dynFalse = do
  loc <- getLoc
  reserved lexer "false"
  return $ DynBool loc False

dynBool :: Parser DynExpr
dynBool = dynTrue <|> dynFalse

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
  loc <- getLoc
  reserved lexer "class"
  cn <- className
  ancestor <- optionMaybe classAncestor
  body <- sepBy dynExpr (many newline)
  reserved lexer "end"
  let thisClass = Class cn ancestor
  return $ DynClassDecl loc thisClass body


dynTerm :: Parser DynExpr
dynTerm =  dynNumber
       <|> dynString
       <|> dynBool
       <|> dynSymbol
       <|> dynList
       <|> try dynIdentifier

----------------------------------------------------------------------
-- Remember, the lookup order does count
dynExpr :: Parser DynExpr
dynExpr =  dynVar
       <|> dynBoolLike
       <|> dynTerm
       <|> dynIf
       <|> dynModuleImport
       <|> dynFunDecl
       <|> dynClassDecl

----------------------------------------------------------------------
dynParensBuried :: Parser DynExpr
dynParensBuried =  parens lexer dynTerm
               <|> dynParensBuried

----------------------------------------------------------------------
dynHashBang :: Parser ()
dynHashBang = optional $ try $ do
  _ <- string "#!"
  skipMany1 (noneOf "\n")
  _ <- newline
  return ()

----------------------------------------------------------------------
rubyFile :: Parser [DynExpr]
rubyFile = do
  let commaP = comma lexer
  dynHashBang
  sepBy1 dynExpr (commaP <|> many1 newline)

----------------------------------------------------------------------
parseRuby :: String -> Either String [DynExpr]
parseRuby s = case parse rubyFile "" s of
  Left err -> Left (show err)
  Right r -> Right r
