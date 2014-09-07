{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Language.MoonRock.Evaluator where

import Language.MoonRock.AST

data AnyVal = forall a. Show a => AnyVal a

instance Show AnyVal where
  show (AnyVal x) = show x

eval :: [DynExpr] -> [AnyVal]
eval [] = []
eval (x:xs) = case x of
  DynBool _ b -> AnyVal b : eval xs
  _ -> error "To be implemented"
