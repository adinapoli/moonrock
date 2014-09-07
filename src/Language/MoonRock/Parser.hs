{-# LANGUAGE OverloadedStrings #-}
module Language.MoonRock.Parser where


import Control.Applicative hiding ((<|>), many)
import Text.Parsec
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
            , identLetter    = alphaNum <|> oneOf ['_']
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
  body <- many dynExpr
  reserved lexer "end"
  return $ DynFunDecl loc name args body


rubyStr :: Parser String
rubyStr = stringLiteral lexer <|>
          (between (char '\'')
                   (char '\'')
                   (many1 (noneOf ['\'', '\n'])))

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
dynNumber =  dynInt
         <|> dynDouble

dynExpr :: Parser DynExpr
dynExpr =  dynNumber
       <|> dynString
       <|> dynModuleImport
       <|> dynIdentifier
       <|> dynFunDecl

rubyFile :: Parser [DynExpr]
rubyFile = sepBy dynExpr (many newline)
