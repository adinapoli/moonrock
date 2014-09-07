
module Language.MoonRock.AST where

import Text.ParserCombinators.Parsec

type Loc = SourcePos

data DNum =
    DInt    Integer
  | DDouble Double
  deriving Show

data DynExpr =
    DynFunDecl Loc String [DynExpr] [DynExpr]
  | DynNum Loc DNum
  | DynString Loc String
  | DynIdentifier Loc String
  | DynModuleImport Loc String
  | DynVar   Loc String DynExpr
  | DynBlock Loc [DynExpr] [DynExpr]
  deriving Show
