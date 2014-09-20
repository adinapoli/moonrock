module Language.MoonRock.AST where

import Text.ParserCombinators.Parsec

type Loc = SourcePos

data DNum =
    DInt    Integer
  | DDouble Double
  deriving (Show, Eq)

data DOp =
    DPlus  DynExpr DynExpr
  | DSub DynExpr DynExpr
  | DMult DynExpr DynExpr
  | DNot DynExpr
  | DEqual DynExpr DynExpr
  | DNEqual DynExpr DynExpr
  | DIf DynExpr [DynExpr] [DynExpr]
  | DAnd DynExpr DynExpr
  | DOr DynExpr DynExpr
  deriving (Show, Eq)

data Class = Class String (Maybe Class) deriving (Show, Eq)

data DynExpr =
    DynFunDecl Loc String [DynExpr] [DynExpr]
  | DynNum Loc DNum
  | DynString Loc String
  | DynBool Loc Bool
  | DynIdentifier Loc String
  | DynSymbol Loc String
  | DynModuleImport Loc String
  | DynVar Loc DynExpr DynExpr
  | DynList Loc [DynExpr]
  | DynOp Loc DOp
  | DynMethodAccess Loc [DynExpr]
  | DynClassDecl Loc Class [DynExpr]
  | DynBlock Loc [DynExpr] [DynExpr]
  deriving (Show, Eq)

data TExpr =
  TBool Loc Bool
  deriving (Show, Eq)
