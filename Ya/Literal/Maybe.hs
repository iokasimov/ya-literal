module Ya.Literal.Maybe where

import Ya

import "base" GHC.IsList (IsList (Item, toList, fromList))

instance IsList (Maybe i) where
 type Item (Maybe i) = i
 fromList [] = Empty `har` Unit
 fromList [x] = Exist `har` x
