module Ya.Literal.String where

import Ya
import Ya.ASCII
import Ya.World

import "base" GHC.Err (error)
import "base" Data.Char (Char)
import "base" Data.String (IsString (fromString))

import Ya.Literal.Utils

instance IsString (List Char) where
 fromString x = T'TT'I (Exist (Build (worker x))) where
  worker (c : []) = Item c `ha` Last `hc` Unit
  worker (c : cs) = Item c `ha` Next `hc` worker cs

instance IsString (List Glyph) where
 fromString [] = T'TT'I (Empty Unit)
 fromString x = T'TT'I (Exist (Build (worker x))) where
  worker (c : []) = Item `hc` glyph_to_ascii c `ha` Last `hc` Unit
  worker (c : cs) = Item `hc` glyph_to_ascii c `ha` Next `hc` worker cs

instance IsString (Construction Optional Glyph) where
 fromString x = Build (worker x) where
  worker (c : []) = Item `hc` glyph_to_ascii c `ha` Last `hc` Unit
  worker (c : cs) = Item `hc` glyph_to_ascii c `ha` Next `hc` worker cs

instance IsString (Construction Optional Letter) where
 fromString x = Build (worker x) where
  worker (c : []) = Item `hc` char_to_letter c `ha` Last `hc` Unit
  worker (c : cs) = Item `hc` char_to_letter c `ha` Next `hc` worker cs

instance IsString (List Letter) where
 fromString x = T'TT'I (Exist (Build (worker x))) where
  worker (c : []) = Item `hc` char_to_letter c `ha` Last `hc` Unit
  worker (c : cs) = Item `hc` char_to_letter c `ha` Next `hc` worker cs

instance IsString (List ASCII) where
 fromString [] = T'TT'I (Empty Unit)
 fromString x = T'TT'I (Exist (Build (worker x))) where
  worker (c : []) = Item `hc` char_to_ascii_with_error c `ha` Last `hc` Unit
  worker (c : cs) = Item `hc` char_to_ascii_with_error c `ha` Next `hc` worker cs

instance IsString (Construction Optional ASCII) where
 fromString x = Build (worker x) where
  worker (c : []) = Item `hc` char_to_ascii_with_error c `ha` Last `hc` Unit
  worker (c : cs) = Item `hc` char_to_ascii_with_error c `ha` Next `hc` worker cs

instance IsString (Construction Optional Unit) where
 fromString x = Build (worker x) where
  worker (_ : []) = Item `hc` Unit `ha` Last `hc` Unit
  worker (_ : cs) = Item `hc` Unit `ha` Next `hc` worker cs

char_to_ascii_with_error x = Empty `hu` error ('<' : x : "> is not ASCII!") `hs` is `hc_` char_to_ascii x