module Ya.Literal.String where

import Ya
import Ya.ASCII
import Ya.World

import "base" Data.Char (Char)
import "base" Data.String (IsString (fromString))

import Ya.Literal.Utils

instance IsString (List Char) where
 fromString x = T'TT'I (Exist (Construct (worker x))) where
  worker (c : []) = Item c `ha` Last `hv` Unit
  worker (c : cs) = Item c `ha` Next `hv` worker cs

instance IsString (List Glyph) where
 fromString [] = T'TT'I (Empty Unit)
 fromString x = T'TT'I (Exist (Construct (worker x))) where
  worker (c : []) = Item `hv` glyph_to_ascii c `ha` Last `hv` Unit
  worker (c : cs) = Item `hv` glyph_to_ascii c `ha` Next `hv` worker cs

instance IsString (Construction Optional Glyph) where
 fromString x = Construct (worker x) where
  worker (c : []) = Item `hv` glyph_to_ascii c `ha` Last `hv` Unit
  worker (c : cs) = Item `hv` glyph_to_ascii c `ha` Next `hv` worker cs

instance IsString (Construction Optional Letter) where
 fromString x = Construct (worker x) where
  worker (c : []) = Item `hv` char_to_letter c `ha` Last `hv` Unit
  worker (c : cs) = Item `hv` char_to_letter c `ha` Next `hv` worker cs

instance IsString (List Letter) where
 fromString x = T'TT'I (Exist (Construct (worker x))) where
  worker (c : []) = Item `hv` char_to_letter c `ha` Last `hv` Unit
  worker (c : cs) = Item `hv` char_to_letter c `ha` Next `hv` worker cs

instance IsString (List ASCII) where
 fromString [] = T'TT'I (Empty Unit)
 fromString x = T'TT'I (Exist (Construct (worker x))) where
  worker (c : []) = Item `hv` char_to_ascii c `ha` Last `hv` Unit
  worker (c : cs) = Item `hv` char_to_ascii c `ha` Next `hv` worker cs

instance IsString (Construction Optional ASCII) where
 fromString x = Construct (worker x) where
  worker (c : []) = Item `hv` char_to_ascii c `ha` Last `hv` Unit
  worker (c : cs) = Item `hv` char_to_ascii c `ha` Next `hv` worker cs

instance IsString (Construction Optional Unit) where
 fromString x = Construct (worker x) where
  worker (_ : []) = Item `hv` Unit `ha` Last `hv` Unit
  worker (_ : cs) = Item `hv` Unit `ha` Next `hv` worker cs
