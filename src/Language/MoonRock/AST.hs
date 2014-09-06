
module Language.MoonRock.AST where


data DNum =
    DInt     Integer
  | DDouble  Double
  deriving Show

data DynExpr =
    DynFun [DynExpr] [DynExpr]
  | DynNum DNum
  | DynString String
  | DynIdent String
  | DynVar   String DynExpr
  | DynBlock [DynExpr] [DynExpr]
  deriving Show
