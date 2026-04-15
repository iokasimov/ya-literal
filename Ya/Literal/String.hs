module Ya.Literal.String where

import Ya
import Ya.ASCII

import "base" GHC.Err (error)
import "base" Data.Char (Char)
import "base" Data.String (IsString (fromString))

import Ya.Literal.Utils

instance IsString (List Char) where
 fromString x = T'TT'I (Exist (Build (worker x))) where
  worker (c : []) = Item c `ha` Last `har` Unit
  worker (c : cs) = Item c `ha` Next `har` worker cs

instance IsString (List Glyph) where
 fromString [] = T'TT'I (Empty Unit)
 fromString x = T'TT'I (Exist (Build (worker x))) where
  worker (c : []) = Item `har` glyph_to_ascii c `ha_` Last `har` Unit
  worker (c : cs) = Item `har` glyph_to_ascii c `ha_` Next `har` worker cs

instance IsString (Construction Optional Glyph) where
 fromString x = Build (worker x) where
  worker (c : []) = Item `har` glyph_to_ascii c `ha_` Last `har` Unit
  worker (c : cs) = Item `har` glyph_to_ascii c `ha_` Next `har` worker cs

instance IsString (Construction Optional Letter) where
 fromString x = Build (worker x) where
  worker (c : []) = Item `har` char_to_letter c `ha_` Last `har` Unit
  worker (c : cs) = Item `har` char_to_letter c `ha_` Next `har` worker cs

instance IsString (List Letter) where
 fromString x = T'TT'I (Exist (Build (worker x))) where
  worker (c : []) = Item `har` char_to_letter c `ha_` Last `har` Unit
  worker (c : cs) = Item `har` char_to_letter c `ha_` Next `har` worker cs

instance IsString (List ASCII) where
 fromString [] = T'TT'I (Empty Unit)
 fromString x = T'TT'I (Exist (Build (worker x))) where
  worker (c : []) = Item `har` char_to_ascii_with_error c `ha_` Last `har` Unit
  worker (c : cs) = Item `har` char_to_ascii_with_error c `ha_` Next `har` worker cs

instance IsString (Construction Optional ASCII) where
 fromString x = Build (worker x) where
  worker (c : []) = Item `har` char_to_ascii_with_error c `ha_` Last `har` Unit
  worker (c : cs) = Item `har` char_to_ascii_with_error c `ha_` Next `har` worker cs

instance IsString (Construction Optional Unit) where
 fromString x = Build (worker x) where
  worker (_ : []) = Item `har` Unit `ha_` Last `har` Unit
  worker (_ : cs) = Item `har` Unit `ha_` Next `har` worker cs

char_to_ascii_with_error x = Empty `ho'ut` error ('<' : x : "> is not ASCII!") `bt'has` is `har` char_to_ascii x
