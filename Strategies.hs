module Strategies
    ( Strategy
    , nthArg
    , returnsFirstArg
    ) where

import Language.Haskell.Exts.Syntax

type Strategy = (Type -> Type)

nthArg :: Int -> Type -> Type
nthArg 1 (TyFun first _) = first
nthArg n (TyFun _ rest) = nthArg (n - 1) rest
nthArg n (TyForall b rs t) = TyForall b rs $ nthArg n t

returnsFirstArg :: Type -> Type
returnsFirstArg (TyFun first rest) = (TyFun (TyVar (Ident "poo")) first)
returnsFirstArg (TyForall x rs t@(TyFun _ _)) = (TyForall x rs (returnsFirstArg t))
