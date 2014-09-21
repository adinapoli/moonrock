{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.MoonRock.AST where

import Text.ParserCombinators.Parsec
import Control.Applicative
import Test.QuickCheck

type Loc = (Line, Column)

data DNum =
    DInt    Integer
  | DDouble Double
  deriving (Show, Eq)

instance Arbitrary DNum where
  arbitrary = oneof [
      DInt <$> arbitrary
    , DDouble <$> arbitrary
    ]

data DOp =
    DPlus DynExpr DynExpr
  | DSub DynExpr DynExpr
  | DMult DynExpr DynExpr
  | DNot DynExpr
  | DEqual DynExpr DynExpr
  | DNEqual DynExpr DynExpr
  | DIf DynExpr [DynExpr] [DynExpr]
  | DAnd DynExpr DynExpr
  | DOr DynExpr DynExpr
  deriving (Show, Eq)

instance Arbitrary DOp where
  arbitrary = oneof [
      DPlus <$> arbitrary <*> arbitrary
    , DSub <$> arbitrary <*> arbitrary
    , DMult <$> arbitrary <*> arbitrary
    , DNot <$> arbitrary 
    , DEqual <$> arbitrary <*> arbitrary
    , DNEqual <$> arbitrary <*> arbitrary
    , DIf <$> arbitrary <*> arbitrary <*> arbitrary
    , DAnd <$> arbitrary <*> arbitrary
    , DOr <$> arbitrary <*> arbitrary
    ]

data Class = Class String (Maybe Class) deriving (Show, Eq)

capitalisedStr :: Gen String
capitalisedStr = do
  cap <- listOf1 $ elements ['A' .. 'Z']
  (head cap :) <$> listOf (elements ['a' .. 'b'])

instance Arbitrary Class where
  arbitrary = Class <$> capitalisedStr <*> arbitrary

data DynExpr =
    DynFunDecl Loc String [DynExpr] [DynExpr]
  | DynNum Loc DNum
  | DynString Loc String
  | DynBool Loc Bool
  | DynNil Loc ()
  | DynIdentifier Loc String
  | DynSymbol Loc String
  | DynModuleImport Loc String
  | DynVar Loc DynExpr DynExpr
  | DynList Loc [DynExpr]
  | DynOp Loc DOp
  | DynMethodAccess Loc [DynExpr]
  | DynClassDecl Loc Class [DynExpr]
--  | DynBlock Loc [DynExpr] [DynExpr]
  deriving (Show, Eq)

instance Arbitrary DynExpr where
  arbitrary = oneof [
      DynFunDecl <$> arbitrary
                 <*> arbitrary
                 <*> arbitrary
                 <*> arbitrary
    , DynNum <$> arbitrary <*> arbitrary
    , DynString <$> arbitrary <*> arbitrary
    , DynNil <$> arbitrary <*> arbitrary
    , DynBool <$> arbitrary <*> arbitrary
    , DynIdentifier <$> arbitrary <*> arbitrary
    , DynSymbol <$> arbitrary <*> arbitrary
    , DynModuleImport <$> arbitrary <*> arbitrary
    , DynVar <$> arbitrary <*> arbitrary <*> arbitrary
    , DynList <$> arbitrary <*> arbitrary
    , DynOp <$> arbitrary <*> arbitrary
    , DynClassDecl <$> arbitrary <*> arbitrary <*> arbitrary
    , DynMethodAccess <$> arbitrary <*> arbitrary
--    , DynBlock <$> arbitrary <*> arbitrary <*> arbitrary
    ]

data TExpr =
  TBool Loc Bool
  deriving (Show, Eq)
