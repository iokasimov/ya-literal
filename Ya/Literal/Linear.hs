{-# LANGUAGE UndecidableInstances #-}
module Ya.Literal.Linear where

import Ya

import GHC.TypeNats

type Scalar = Alone

type family Vector n where
 Vector 1 = Scalar
 Vector n = Vector (n - 1) `P'T'I'TT'I` Scalar

pattern Vector :: forall n i . Vector n i `AR__` Vector n i
pattern Vector x = x

type Matrix n m = Vector m `T'TT'I` Vector n

pattern Matrix :: forall n m i . Matrix n m i `AR__` Matrix n m i
pattern Matrix x = x

type family Tensor n where
 Tensor '[] = Scalar
 Tensor (n ': '[]) = Vector n
 Tensor (n ': m) = Tensor m `T'TT'I` Vector n

pattern Tensor :: forall n i . Tensor n i `AR__` Tensor n i
pattern Tensor x = x

type family N x where
 N 1 = Unit
 N n = N (n - 1) `S` Unit

type family Shape x where
 Shape Alone = Unit
 Shape (t `P'T'I'TT'I` tt) = Shape t `S` Shape tt
 Shape (t `T'TT'I` tt) = Shape t `P` Shape tt

pattern Shape :: forall t . Shape t `AR__` Shape t
pattern Shape x = x

type Slots n = Shape (Tensor n)

pattern Slots :: forall n . Slots n `AR__` Slots n
pattern Slots x = x
