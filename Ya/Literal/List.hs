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

instance Show item => Show (List item) where
 show = show `ha` toList

instance IsList (Construction Optional item) where
 type Item (Construction Optional item) = item
 fromList x = Construct (worker x) where
  worker (c : []) = Item c `ha` Last `hv` Unit
  worker (c : cs) = Item c `ha` Next `hv` worker cs

instance IsList (List item) where
 type Item (List item) = item
 fromList [] = empty @List
 fromList xs = List (worker xs) where
  worker (c : []) = Item c `ha` Last `hv` Unit
  worker (c : cs) = Item c `ha` Next `hv` worker cs
 toList xs = xs
  `yokl` Prior `ha` Apply `ha` State `ha` Event `ha_` (:) `ho'ho` get
  `he'he'hv___` []
  `yi__` that @[_]

instance IsList (Shafted List item) where
 type Item (Shafted List item) = [item]
 fromList [sx,xs] = T'TT'I'TTT'I (Label (fromList @(List item) (reverse sx)) `lu` Label (fromList @(List item) xs))

instance IsList ((Alone `P'T'I'TT'I` Shafted List) item) where
 type Item ((Alone `P'T'I'TT'I` Shafted List) item) = [item]
 fromList [sx,[x],xs] = T'TT'I'TTT'I (Alone x `lu` T'TT'I'TTT'I (Label (fromList @(List item) (reverse sx)) `lu` Label (fromList @(List item) xs)))

instance IsList ((List `P'T'I'TT'I` Shafted List) item) where
 type Item ((List `P'T'I'TT'I` Shafted List) item) = [item]
 fromList [sx,x,xs] = T'TT'I'TTT'I (fromList @(List item) x `lu` T'TT'I'TTT'I (Label (fromList @(List item) (reverse sx)) `lu` Label (fromList @(List item) xs)))

-- instance IsList (Construction Optional `T'TT'I` Along k `T'I_` item) where
--  type Item (Construction Optional `T'TT'I` Along k `T'I_` item) = (k, item)
--  fromList x = T'TT'I (Construct (worker x)) where
--   worker (c : []) = Item c `ha` Last `hv` Unit
--   worker (c : cs) = Item c `ha` Next `hv` worker cs

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
