
module Language.MoonRock.AST where

import Text.ParserCombinators.Parsec

type Loc = SourcePos

data DNum =
    DInt    Integer
  | DDouble Double
  deriving Show

-- A class is modeled as as loosely-coupled
-- Rose tree of ancestors.
data Class = Class String (Maybe Class) deriving Show

data DynExpr =
    DynFunDecl Loc String [DynExpr] [DynExpr]
  | DynNum Loc DNum
  | DynString Loc String
  | DynBool Loc Bool
  | DynIdentifier Loc String
  | DynSymbol Loc String
  | DynModuleImport Loc String
  | DynVar Loc DynExpr DynExpr
  | DynMethodAccess Loc [DynExpr]
  | DynClassDecl Loc Class [DynExpr]
  | DynBlock Loc [DynExpr] [DynExpr]
  deriving Show
