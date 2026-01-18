module Ya.Literal.Utils where

import Ya
import Ya.ASCII
import Ya.World

import "base" Data.Char (Char)
import "base" Data.String (IsString (fromString))
import "base" GHC.Err (error)

char_to_letter :: Char -> Letter
char_to_letter = \case
 'A' -> Upper `hc'he` A
 'B' -> Upper `hc'he` B
 'C' -> Upper `hc'he` C
 'D' -> Upper `hc'he` D
 'E' -> Upper `hc'he` E
 'F' -> Upper `hc'he` F
 'G' -> Upper `hc'he` G
 'H' -> Upper `hc'he` H
 'I' -> Upper `hc'he` I
 'J' -> Upper `hc'he` J
 'K' -> Upper `hc'he` K
 'L' -> Upper `hc'he` L
 'M' -> Upper `hc'he` M
 'N' -> Upper `hc'he` N
 'O' -> Upper `hc'he` O
 'P' -> Upper `hc'he` P
 'Q' -> Upper `hc'he` Q
 'R' -> Upper `hc'he` R
 'S' -> Upper `hc'he` S
 'T' -> Upper `hc'he` T
 'U' -> Upper `hc'he` U
 'V' -> Upper `hc'he` V
 'W' -> Upper `hc'he` W
 'X' -> Upper `hc'he` X
 'Y' -> Upper `hc'he` Y
 'Z' -> Upper `hc'he` Z
 'a' -> Lower `hc'he` A
 'b' -> Lower `hc'he` B
 'c' -> Lower `hc'he` C
 'd' -> Lower `hc'he` D
 'e' -> Lower `hc'he` E
 'f' -> Lower `hc'he` F
 'g' -> Lower `hc'he` G
 'h' -> Lower `hc'he` H
 'i' -> Lower `hc'he` I
 'j' -> Lower `hc'he` J
 'k' -> Lower `hc'he` K
 'l' -> Lower `hc'he` L
 'm' -> Lower `hc'he` M
 'n' -> Lower `hc'he` N
 'o' -> Lower `hc'he` O
 'p' -> Lower `hc'he` P
 'q' -> Lower `hc'he` Q
 'r' -> Lower `hc'he` R
 's' -> Lower `hc'he` S
 't' -> Lower `hc'he` T
 'u' -> Lower `hc'he` U
 'v' -> Lower `hc'he` V
 'w' -> Lower `hc'he` W
 'x' -> Lower `hc'he` X
 x -> error "Not a latin character!"

caret_to_char :: Caret -> Char
caret_to_char = is `hu` '\HT' `hs` is `hu` '\LF' `hs` is `hu` '\ESC' `hs` is `hu` '\BS' `hs` is `hu` '\DEL'

bracket_to_char :: Bracket -> Char
bracket_to_char = is `hu` '(' `hs` is `hu` '{' `hs` is `hu` '<' `hs` is `hu` '['
              `hs_` is `hu` ')' `hs` is `hu` '}' `hs` is `hu` '>' `hs` is `hu` ']'

punctuation_to_char :: Punctuate -> Char
punctuation_to_char = is `hu` '\"' `hs` is `hu` '\'' `hs` is `hu` '#' `hs` is `hu` '=' `hs` is `hu` '-' `hs` is `hu` '@'
 `hs` is `hu` '^' `hs` is `hu` '_' `hs` is `hu` '`' `hs` is `hu` '|' `hs` is `hu` '~'
 `hs` is `hu` '+' `hs` is `hu` '*' `hs` is `hu` '%' `hs` is `hu` '&' `hs` is `hu` '$' `hs` is `hu` '\\' `hs` is `hu` '/'
 `hs` is `hu` '.' `hs` is `hu` ',' `hs` is `hu` ';' `hs` is `hu` ':' `hs` is `hu` '!' `hs` is `hu` '?' `hs` is `hu` ' '

upper_latin_to_char :: Latin -> Char
upper_latin_to_char = is `hu` 'A' `hs` is `hu` 'B' `hs` is `hu` 'C' `hs` is `hu` 'D' `hs` is `hu` 'E' `hs` is `hu` 'F'
 `hs` is `hu` 'G' `hs` is `hu` 'H' `hs` is `hu` 'I' `hs` is `hu` 'J' `hs` is `hu` 'K' `hs` is `hu` 'L'
 `hs` is `hu` 'M' `hs` is `hu` 'N' `hs` is `hu` 'O' `hs` is `hu` 'P' `hs` is `hu` 'Q' `hs` is `hu` 'R'
 `hs` is `hu` 'S' `hs` is `hu` 'T' `hs` is `hu` 'U' `hs` is `hu` 'V' `hs` is `hu` 'W' `hs` is `hu` 'X'
 `hs` is `hu` 'Y' `hs` is `hu` 'Z'

lower_latin_to_char :: Latin -> Char
lower_latin_to_char = is `hu` 'a' `hs` is `hu` 'b' `hs` is `hu` 'c' `hs` is `hu` 'd' `hs` is `hu` 'e' `hs` is `hu` 'f'
 `hs` is `hu` 'g' `hs` is `hu` 'h' `hs` is `hu` 'i' `hs` is `hu` 'j' `hs` is `hu` 'k' `hs` is `hu` 'l'
 `hs` is `hu` 'm' `hs` is `hu` 'n' `hs` is `hu` 'o' `hs` is `hu` 'p' `hs` is `hu` 'q' `hs` is `hu` 'r'
 `hs` is `hu` 's' `hs` is `hu` 't' `hs` is `hu` 'u' `hs` is `hu` 'v' `hs` is `hu` 'w' `hs` is `hu` 'x'
 `hs` is `hu` 'y' `hs` is `hu` 'z'

digit_to_char :: Digit -> Char
digit_to_char = is `hu` '0' `hs` is `hu` '1' `hs` is `hu` '2' `hs` is `hu` '3' `hs` is `hu` '4' `hs` is `hu` '5' `hs` is `hu` '6' `hs` is `hu` '7' `hs` is `hu` '8' `hs` is `hu` '9'

ascii_to_char :: ASCII -> Char
ascii_to_char = is
 `hc_` lower_latin_to_char
  `hs` upper_latin_to_char
 `hs_` digit_to_char `ha` is
 `hs_` bracket_to_char `ha` is
  `hs` punctuation_to_char `ha` is
 `hs_` caret_to_char `ha` is

glyph_to_ascii = \case
 ' ' -> Symbol `ha` Punctuate `hc'he` Space
 '/' -> Symbol `ha` Punctuate `hc'he` (Back `ha` Slash)
 '\\' -> Symbol `ha` Punctuate `hc'he` Slash
 '(' -> Symbol `ha` Bracket `ha` Opened `hc'he` Round
 ')' -> Symbol `ha` Bracket `ha` Closed `hc'he` Round
 '{' -> Symbol `ha` Bracket `ha` Opened `hc'he` Curly
 '}' -> Symbol `ha` Bracket `ha` Closed `hc'he` Curly
 '<' -> Symbol `ha` Bracket `ha` Opened `hc'he` Angle
 '>' -> Symbol `ha` Bracket `ha` Closed `hc'he` Angle
 '[' -> Symbol `ha` Bracket `ha` Opened `hc'he` Square
 ']' -> Symbol `ha` Bracket `ha` Closed `hc'he` Square
 '"' -> Symbol `ha` Punctuate `hc'he` Doublequote
 '\'' -> Symbol `ha` Punctuate `hc'he` Singlequote
 '.' -> Symbol `ha` Punctuate `hc'he` Period
 ',' -> Symbol `ha` Punctuate `hc'he` Comma
 ';' -> Symbol `ha` Punctuate `hc'he` Semicolon
 ':' -> Symbol `ha` Punctuate `hc'he` Colon
 '!' -> Symbol `ha` Punctuate `hc'he` Exclam
 '?' -> Symbol `ha` Punctuate `hc'he` Question
 '#' -> Symbol `ha` Punctuate `hc'he` Hash
 '$' -> Symbol `ha` Punctuate `hc'he` Dollar
 '%' -> Symbol `ha` Punctuate `hc'he` Percent
 '&' -> Symbol `ha` Punctuate `hc'he` Ampersand
 '*' -> Symbol `ha` Punctuate `hc'he` Asterisk
 '+' -> Symbol `ha` Punctuate `hc'he` Plus
 '-' -> Symbol `ha` Punctuate `hc'he` Hyphen
 '=' -> Symbol `ha` Punctuate `hc'he` Equality
 '@' -> Symbol `ha` Punctuate `hc'he` At
 '^' -> Symbol `ha` Punctuate `hc'he` Circumflex
 '_' -> Symbol `ha` Punctuate `hc'he` Underscore
 '`' -> Symbol `ha` Punctuate `hc'he` Grave
 '|' -> Symbol `ha` Punctuate `hc'he` Bar
 '~' -> Symbol `ha` Punctuate `hc'he` Tilde
 'A' -> Letter `ha` Upper `hc'he` A
 'B' -> Letter `ha` Upper `hc'he` B
 'C' -> Letter `ha` Upper `hc'he` C
 'D' -> Letter `ha` Upper `hc'he` D
 'E' -> Letter `ha` Upper `hc'he` E
 'F' -> Letter `ha` Upper `hc'he` F
 'G' -> Letter `ha` Upper `hc'he` G
 'H' -> Letter `ha` Upper `hc'he` H
 'I' -> Letter `ha` Upper `hc'he` I
 'J' -> Letter `ha` Upper `hc'he` J
 'K' -> Letter `ha` Upper `hc'he` K
 'L' -> Letter `ha` Upper `hc'he` L
 'M' -> Letter `ha` Upper `hc'he` M
 'N' -> Letter `ha` Upper `hc'he` N
 'O' -> Letter `ha` Upper `hc'he` O
 'P' -> Letter `ha` Upper `hc'he` P
 'Q' -> Letter `ha` Upper `hc'he` Q
 'R' -> Letter `ha` Upper `hc'he` R
 'S' -> Letter `ha` Upper `hc'he` S
 'T' -> Letter `ha` Upper `hc'he` T
 'U' -> Letter `ha` Upper `hc'he` U
 'V' -> Letter `ha` Upper `hc'he` V
 'W' -> Letter `ha` Upper `hc'he` W
 'X' -> Letter `ha` Upper `hc'he` X
 'Y' -> Letter `ha` Upper `hc'he` Y
 'Z' -> Letter `ha` Upper `hc'he` Z
 'a' -> Letter `ha` Lower `hc'he` A
 'b' -> Letter `ha` Lower `hc'he` B
 'c' -> Letter `ha` Lower `hc'he` C
 'd' -> Letter `ha` Lower `hc'he` D
 'e' -> Letter `ha` Lower `hc'he` E
 'f' -> Letter `ha` Lower `hc'he` F
 'g' -> Letter `ha` Lower `hc'he` G
 'h' -> Letter `ha` Lower `hc'he` H
 'i' -> Letter `ha` Lower `hc'he` I
 'j' -> Letter `ha` Lower `hc'he` J
 'k' -> Letter `ha` Lower `hc'he` K
 'l' -> Letter `ha` Lower `hc'he` L
 'm' -> Letter `ha` Lower `hc'he` M
 'n' -> Letter `ha` Lower `hc'he` N
 'o' -> Letter `ha` Lower `hc'he` O
 'p' -> Letter `ha` Lower `hc'he` P
 'q' -> Letter `ha` Lower `hc'he` Q
 'r' -> Letter `ha` Lower `hc'he` R
 's' -> Letter `ha` Lower `hc'he` S
 't' -> Letter `ha` Lower `hc'he` T
 'u' -> Letter `ha` Lower `hc'he` U
 'v' -> Letter `ha` Lower `hc'he` V
 'w' -> Letter `ha` Lower `hc'he` W
 'x' -> Letter `ha` Lower `hc'he` X
 'y' -> Letter `ha` Lower `hc'he` Y
 'z' -> Letter `ha` Lower `hc'he` Z
 '0' -> Digit `hc'he` Zero
 '1' -> Digit `hc'he` One
 '2' -> Digit `hc'he` Two
 '3' -> Digit `hc'he` Three
 '4' -> Digit `hc'he` Four
 '5' -> Digit `hc'he` Five
 '6' -> Digit `hc'he` Six
 '7' -> Digit `hc'he` Seven
 '8' -> Digit `hc'he` Eight
 '9' -> Digit `hc'he` Nine
 _ -> error "Not a Glyph!"

char_to_ascii = \case
 '\BS' -> Exist `ha` Caret `hc'he` (Back `ha` Space)
 '\HT' -> Exist `ha` Caret `hc'he` Tab
 '\LF' -> Exist `ha` Caret `hc'he` Newline
 '\ESC' -> Exist `ha` Caret `hc'he` Escape
 ' ' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Space
 '\DEL' -> Exist `ha` Caret `hc'he` Delete
 '/' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` (Back `ha` Slash)
 '\\' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Slash
 '(' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hc'he` Round
 ')' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hc'he` Round
 '{' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hc'he` Curly
 '}' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hc'he` Curly
 '<' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hc'he` Angle
 '>' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hc'he` Angle
 '[' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hc'he` Square
 ']' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hc'he` Square
 '"' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Doublequote
 '\'' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Singlequote
 '.' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Period
 ',' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Comma
 ';' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Semicolon
 ':' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Colon
 '!' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Exclam
 '?' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Question
 '#' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Hash
 '$' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Dollar
 '%' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Percent
 '&' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Ampersand
 '*' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Asterisk
 '+' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Plus
 '-' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Hyphen
 '=' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Equality
 '@' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` At
 '^' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Circumflex
 '_' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Underscore
 '`' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Grave
 '|' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Bar
 '~' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'he` Tilde
 'A' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` A
 'B' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` B
 'C' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` C
 'D' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` D
 'E' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` E
 'F' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` F
 'G' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` G
 'H' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` H
 'I' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` I
 'J' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` J
 'K' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` K
 'L' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` L
 'M' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` M
 'N' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` N
 'O' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` O
 'P' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` P
 'Q' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` Q
 'R' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` R
 'S' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` S
 'T' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` T
 'U' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` U
 'V' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` V
 'W' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` W
 'X' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` X
 'Y' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` Y
 'Z' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'he` Z
 'a' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` A
 'b' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` B
 'c' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` C
 'd' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` D
 'e' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` E
 'f' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` F
 'g' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` G
 'h' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` H
 'i' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` I
 'j' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` J
 'k' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` K
 'l' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` L
 'm' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` M
 'n' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` N
 'o' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` O
 'p' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` P
 'q' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` Q
 'r' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` R
 's' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` S
 't' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` T
 'u' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` U
 'v' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` V
 'w' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` W
 'x' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` X
 'y' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` Y
 'z' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'he` Z
 '0' -> Exist `ha` Glyph `ha` Digit `hc'he` Zero
 '1' -> Exist `ha` Glyph `ha` Digit `hc'he` One
 '2' -> Exist `ha` Glyph `ha` Digit `hc'he` Two
 '3' -> Exist `ha` Glyph `ha` Digit `hc'he` Three
 '4' -> Exist `ha` Glyph `ha` Digit `hc'he` Four
 '5' -> Exist `ha` Glyph `ha` Digit `hc'he` Five
 '6' -> Exist `ha` Glyph `ha` Digit `hc'he` Six
 '7' -> Exist `ha` Glyph `ha` Digit `hc'he` Seven
 '8' -> Exist `ha` Glyph `ha` Digit `hc'he` Eight
 '9' -> Exist `ha` Glyph `ha` Digit `hc'he` Nine
 _ -> super Empty
