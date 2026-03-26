{-# LANGUAGE UndecidableInstances #-}
module Ya.Literal.Linear where

import Ya

import GHC.TypeNats

type Scalar = I

pattern Scalar :: e `AR` Scalar e
pattern Scalar e = Identity e

type family Vector n where
 Vector 1 = I
 Vector n = Vector (n - 1) `P'T'I'TT'I` I

type Matrix n m = Vector n `T'TT'I` Vector m

type family Tensor n where
 Tensor '[] = Scalar
 Tensor (n ': '[]) = Vector n
 Tensor (n ': m) = Vector n `T'TT'I` Tensor m
