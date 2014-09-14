{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Language.MoonRock.Evaluator where

import Language.MoonRock.AST
import Language.MoonRock.Pretty

eval :: [DynExpr] -> [DynExpr]
eval [] = []
eval (x:xs) = case x of
  DynBool l b -> DynBool l b : eval xs
  DynString l s -> DynString l s : eval xs
  DynNum l n -> DynNum l n : eval xs
  DynSymbol l s -> DynSymbol l s : eval xs
  _ -> x : eval xs
