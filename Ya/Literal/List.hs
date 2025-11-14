module Ya.Literal.List where

import Ya
import Ya.ASCII
import Ya.World

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
  worker (c : []) = Item c `ha` Last `hv` Unit
  worker (c : cs) = Item c `ha` Next `hv` worker cs

instance IsList (List i) where
 type Item (List i) = i
 fromList [] = empty @List
 fromList xs = List (Exist (Build (worker xs))) where
  worker (c : []) = Item c `ha` Last `hv` Unit
  worker (c : cs) = Item c `ha` Next `hv` worker cs
 toList xs = xs
  `yokl` Prior `ha` Apply `ha` State `ha` Event `ha_` (:) `ho'ho` fetch
  `he'he'hv___` []
  `yi__` that @[_]

instance IsList (Shafted List i) where
 type Item (Shafted List i) = [i]
 fromList [sx,xs] = fromList @(List i) (reverse sx) `lu` fromList @(List i) xs

instance IsList ((Alone `P'T'I'TT'I` Shafted List) i) where
 type Item ((Alone `P'T'I'TT'I` Shafted List) i) = [i]
 fromList [sx,[x],xs] = Alone x `lu` (fromList @(List i) (reverse sx) `lu` fromList @(List i) xs)

instance IsList ((List `P'T'I'TT'I` Shafted List) i) where
 type Item ((List `P'T'I'TT'I` Shafted List) i) = [i]
 fromList [sx,x,xs] = T'TT'I'TTT'I (fromList @(List i) x `lu` (fromList @(List i) (reverse sx) `lu` fromList @(List i) xs))

instance IsList (Construction Optional `T'TT'I` Along k `T'I_` i) where
 type Item (Construction Optional `T'TT'I` Along k `T'I_` i) = (k, i)
 fromList x = T'TT'I (Build (worker x)) where
  worker ((k,c) : []) = Item (c `lu` k `yi` Along) `ha` Last `hv` Unit
  worker ((k,c) : kcs) = Item (c `lu` k `yi` Along) `ha` Next `hv` worker kcs

instance IsList (List `T'TT'I` Along k `T'I_` i) where
 type Item (List `T'TT'I` Along k `T'I_` i) = (k, i)
 fromList x = T'TT'I (List (Exist (Build (worker x)))) where
  worker ((k,c) : []) = Item (c `lu` k `yi` Along) `ha` Last `hv` Unit
  worker ((k,c) : kcs) = Item (c `lu` k `yi` Along) `ha` Next `hv` worker kcs

integer :: Integer -> Nonempty List Digit
integer = show `ho` fromList `ho'yo` digit where

 digit '0' = by Zero
 digit '1' = by One
 digit '2' = by Two
 digit '3' = by Three
 digit '4' = by Four
 digit '5' = by Five
 digit '6' = by Six
 digit '7' = by Seven
 digit '8' = by Eight
 digit '9' = by Nine
 digit _ = error "Not a digit!"
