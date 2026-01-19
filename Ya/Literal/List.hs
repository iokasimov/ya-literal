module Ya.Literal.List where

import Ya
import Ya.ASCII

import "base" GHC.IsList (IsList (Item, toList, fromList))
import "base" GHC.Integer (Integer)
import "base" GHC.Err (error)
import "base" GHC.List (reverse)
import "base" Text.Show (Show (show))

import Ya.Literal.Utils

instance Show i => Show (List i) where
 show = show `ha` toList

instance IsList (Construction Optional i) where
 type Item (Construction Optional i) = i
 fromList x = Build (worker x) where
  worker (c : []) = Item c `ha` Last `hc` Unit
  worker (c : cs) = Item c `ha` Next `hc` worker cs

instance IsList (List i) where
 type Item (List i) = i
 fromList [] = empty @List
 fromList xs = List (Exist (Build (worker xs))) where
  worker (c : []) = Item c `ha` Last `hc` Unit
  worker (c : cs) = Item c `ha` Next `hc` worker cs
 toList xs = that @[_] `hc____` xs
  `yokl` Prior `ha` Apply `ha` State `ha` Event `ha_` (:) `ho'ho` fetch
  `hc___` []

instance IsList (Twice `T'TT'I` List `T'I_` i) where
 type Item (Twice `T'TT'I` List `T'I_` i) = [i]
 fromList [sx,xs] = fromList @(List i) (reverse sx) `hjd` fromList @(List i) xs

instance IsList ((Alone `P'T'I'TT'I` Twice `T'TT'I` List) i) where
 type Item ((Alone `P'T'I'TT'I` Twice `T'TT'I` List) i) = [i]
 fromList [sx,[x],xs] = Alone x `hjd` (fromList @(List i) (reverse sx) `hjd` fromList @(List i) xs)

instance IsList ((List `P'T'I'TT'I` Twice `T'TT'I` List) i) where
 type Item ((List `P'T'I'TT'I` Twice `T'TT'I` List) i) = [i]
 fromList [sx,x,xs] = T'TT'I'TTT'I (fromList @(List i) x `hjd` (fromList @(List i) (reverse sx) `hjd` fromList @(List i) xs))

instance IsList (Construction Optional `T'TT'I` Along k `T'I_` i) where
 type Item (Construction Optional `T'TT'I` Along k `T'I_` i) = (k, i)
 fromList x = T'TT'I (Build (worker x)) where
  worker ((k,c) : []) = Item (c `hjd` k) `ha` Last `hc` Unit
  worker ((k,c) : kcs) = Item (c `hjd` k) `ha` Next `hc` worker kcs

instance IsList (List `T'TT'I` Along k `T'I_` i) where
 type Item (List `T'TT'I` Along k `T'I_` i) = (k, i)
 fromList x = T'TT'I (List (Exist (Build (worker x)))) where
  worker ((k,c) : []) = Item (c `hjd` k) `ha` Last `hc` Unit
  worker ((k,c) : kcs) = Item (c `hjd` k) `ha` Next `hc` worker kcs

-- instance IsList (Construction List i) where
 -- type Item (Construction List i) = (i, Tree (List i))

integer :: Integer -> Nonempty List Digit
integer = show `ho` fromList `ho'yo` digit where

 digit '0' = super Zero
 digit '1' = super One
 digit '2' = super Two
 digit '3' = super Three
 digit '4' = super Four
 digit '5' = super Five
 digit '6' = super Six
 digit '7' = super Seven
 digit '8' = super Eight
 digit '9' = super Nine
 digit _ = error "Not a digit!"
