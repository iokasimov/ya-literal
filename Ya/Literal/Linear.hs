{-# LANGUAGE UndecidableInstances #-}
module Ya.Literal.Linear where

import Ya

import GHC.TypeNats

type Scalar = Alone

type family Vector n where
 Vector 1 = Scalar
 Vector n = Vector (n - 1) `P'T'I'TT'I` Scalar

type Matrix n m = Vector n `T'TT'I` Vector m

type family Tensor n where
 Tensor '[] = Scalar
 Tensor (n ': '[]) = Vector n
 Tensor (n ': m) = Vector n `T'TT'I` Tensor m
