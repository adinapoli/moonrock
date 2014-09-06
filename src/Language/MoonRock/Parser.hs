{-# LANGUAGE OverloadedStrings #-}
module Language.MoonRock.Parser where


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
            , identLetter    = alphaNum
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
                "nil","not","or","redo",
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

dynString :: Parser DynExpr
dynString = fmap DynString (stringLiteral lexer)

dynNumber :: Parser DynExpr
dynNumber =
  fmap (DynNum . DInt) (integer lexer) <|>
  fmap (DynNum . DDouble) (float lexer)

dynExpr :: Parser DynExpr
dynExpr =  dynNumber
       <|> dynString
