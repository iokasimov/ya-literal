module Ya.Literal.Utils where

import Ya
import Ya.ASCII
import Ya.World

import "base" Data.Char (Char)
import "base" Data.String (IsString (fromString))
import "base" GHC.Err (error)

char_to_letter :: Char -> Letter
char_to_letter = \case
 'A' -> Upper `hv'he` A
 'B' -> Upper `hv'he` B
 'C' -> Upper `hv'he` C
 'D' -> Upper `hv'he` D
 'E' -> Upper `hv'he` E
 'F' -> Upper `hv'he` F
 'G' -> Upper `hv'he` G
 'H' -> Upper `hv'he` H
 'I' -> Upper `hv'he` I
 'J' -> Upper `hv'he` J
 'K' -> Upper `hv'he` K
 'L' -> Upper `hv'he` L
 'M' -> Upper `hv'he` M
 'N' -> Upper `hv'he` N
 'O' -> Upper `hv'he` O
 'P' -> Upper `hv'he` P
 'Q' -> Upper `hv'he` Q
 'R' -> Upper `hv'he` R
 'S' -> Upper `hv'he` S
 'T' -> Upper `hv'he` T
 'U' -> Upper `hv'he` U
 'V' -> Upper `hv'he` V
 'W' -> Upper `hv'he` W
 'X' -> Upper `hv'he` X
 'Y' -> Upper `hv'he` Y
 'Z' -> Upper `hv'he` Z
 'a' -> Lower `hv'he` A
 'b' -> Lower `hv'he` B
 'c' -> Lower `hv'he` C
 'd' -> Lower `hv'he` D
 'e' -> Lower `hv'he` E
 'f' -> Lower `hv'he` F
 'g' -> Lower `hv'he` G
 'h' -> Lower `hv'he` H
 'i' -> Lower `hv'he` I
 'j' -> Lower `hv'he` J
 'k' -> Lower `hv'he` K
 'l' -> Lower `hv'he` L
 'm' -> Lower `hv'he` M
 'n' -> Lower `hv'he` N
 'o' -> Lower `hv'he` O
 'p' -> Lower `hv'he` P
 'q' -> Lower `hv'he` Q
 'r' -> Lower `hv'he` R
 's' -> Lower `hv'he` S
 't' -> Lower `hv'he` T
 'u' -> Lower `hv'he` U
 'v' -> Lower `hv'he` V
 'w' -> Lower `hv'he` W
 'x' -> Lower `hv'he` X
 x -> error "Not a latin character!"

caret_to_char :: Caret -> Char
caret_to_char = is `hu` '\HT' `la` is `hu` '\LF' `la` is `hu` '\ESC' `la` is `hu` '\BS' `la` is `hu` '\DEL'

bracket_to_char :: Bracket -> Char
bracket_to_char = is `hu` '(' `la` is `hu` '{' `la` is `hu` '<' `la` is `hu` '['
              `la_` is `hu` ')' `la` is `hu` '}' `la` is `hu` '>' `la` is `hu` ']'

punctuation_to_char :: Punctuate -> Char
punctuation_to_char = is `hu` '\"' `la` is `hu` '\'' `la` is `hu` '#' `la` is `hu` '=' `la` is `hu` '-' `la` is `hu` '@'
 `la` is `hu` '^' `la` is `hu` '_' `la` is `hu` '`' `la` is `hu` '|' `la` is `hu` '~'
 `la` is `hu` '+' `la` is `hu` '*' `la` is `hu` '%' `la` is `hu` '&' `la` is `hu` '$' `la` is `hu` '\\' `la` is `hu` '/'
 `la` is `hu` '.' `la` is `hu` ',' `la` is `hu` ';' `la` is `hu` ':' `la` is `hu` '!' `la` is `hu` '?' `la` is `hu` ' '

upper_latin_to_char :: Latin -> Char
upper_latin_to_char = is `hu` 'A' `la` is `hu` 'B' `la` is `hu` 'C' `la` is `hu` 'D' `la` is `hu` 'E' `la` is `hu` 'F'
 `la` is `hu` 'G' `la` is `hu` 'H' `la` is `hu` 'I' `la` is `hu` 'J' `la` is `hu` 'K' `la` is `hu` 'L'
 `la` is `hu` 'M' `la` is `hu` 'N' `la` is `hu` 'O' `la` is `hu` 'P' `la` is `hu` 'Q' `la` is `hu` 'R'
 `la` is `hu` 'S' `la` is `hu` 'T' `la` is `hu` 'U' `la` is `hu` 'V' `la` is `hu` 'W' `la` is `hu` 'X'
 `la` is `hu` 'Y' `la` is `hu` 'Z'

lower_latin_to_char :: Latin -> Char
lower_latin_to_char = is `hu` 'a' `la` is `hu` 'b' `la` is `hu` 'c' `la` is `hu` 'd' `la` is `hu` 'e' `la` is `hu` 'f'
 `la` is `hu` 'g' `la` is `hu` 'h' `la` is `hu` 'i' `la` is `hu` 'j' `la` is `hu` 'k' `la` is `hu` 'l'
 `la` is `hu` 'm' `la` is `hu` 'n' `la` is `hu` 'o' `la` is `hu` 'p' `la` is `hu` 'q' `la` is `hu` 'r'
 `la` is `hu` 's' `la` is `hu` 't' `la` is `hu` 'u' `la` is `hu` 'v' `la` is `hu` 'w' `la` is `hu` 'x'
 `la` is `hu` 'y' `la` is `hu` 'z'

digit_to_char :: Digit -> Char
digit_to_char = is `hu` '0' `la` is `hu` '1' `la` is `hu` '2' `la` is `hu` '3' `la` is `hu` '4' `la` is `hu` '5' `la` is `hu` '6' `la` is `hu` '7' `la` is `hu` '8' `la` is `hu` '9'

ascii_to_char :: ASCII -> Char
ascii_to_char = is
  `li` lower_latin_to_char
  `la` upper_latin_to_char
 `la_` digit_to_char `ha` is
 `la_` bracket_to_char `ha` is
  `la` punctuation_to_char `ha` is
 `la_` caret_to_char `ha` is

glyph_to_ascii = \case
 ' ' -> Symbol `ha` Punctuate `hv'he` Space
 '/' -> Symbol `ha` Punctuate `hv'he` (Back `ha` Slash)
 '\\' -> Symbol `ha` Punctuate `hv'he` Slash
 '(' -> Symbol `ha` Bracket `ha` Opened `hv'he` Round
 ')' -> Symbol `ha` Bracket `ha` Closed `hv'he` Round
 '{' -> Symbol `ha` Bracket `ha` Opened `hv'he` Curly
 '}' -> Symbol `ha` Bracket `ha` Closed `hv'he` Curly
 '<' -> Symbol `ha` Bracket `ha` Opened `hv'he` Angle
 '>' -> Symbol `ha` Bracket `ha` Closed `hv'he` Angle
 '[' -> Symbol `ha` Bracket `ha` Opened `hv'he` Square
 ']' -> Symbol `ha` Bracket `ha` Closed `hv'he` Square
 '"' -> Symbol `ha` Punctuate `hv'he` Doublequote
 '\'' -> Symbol `ha` Punctuate `hv'he` Singlequote
 '.' -> Symbol `ha` Punctuate `hv'he` Period
 ',' -> Symbol `ha` Punctuate `hv'he` Comma
 ';' -> Symbol `ha` Punctuate `hv'he` Semicolon
 ':' -> Symbol `ha` Punctuate `hv'he` Colon
 '!' -> Symbol `ha` Punctuate `hv'he` Exclam
 '?' -> Symbol `ha` Punctuate `hv'he` Question
 '#' -> Symbol `ha` Punctuate `hv'he` Hash
 '$' -> Symbol `ha` Punctuate `hv'he` Dollar
 '%' -> Symbol `ha` Punctuate `hv'he` Percent
 '&' -> Symbol `ha` Punctuate `hv'he` Ampersand
 '*' -> Symbol `ha` Punctuate `hv'he` Asterisk
 '+' -> Symbol `ha` Punctuate `hv'he` Plus
 '-' -> Symbol `ha` Punctuate `hv'he` Hyphen
 '=' -> Symbol `ha` Punctuate `hv'he` Equality
 '@' -> Symbol `ha` Punctuate `hv'he` At
 '^' -> Symbol `ha` Punctuate `hv'he` Circumflex
 '_' -> Symbol `ha` Punctuate `hv'he` Underscore
 '`' -> Symbol `ha` Punctuate `hv'he` Grave
 '|' -> Symbol `ha` Punctuate `hv'he` Bar
 '~' -> Symbol `ha` Punctuate `hv'he` Tilde
 'A' -> Letter `ha` Upper `hv'he` A
 'B' -> Letter `ha` Upper `hv'he` B
 'C' -> Letter `ha` Upper `hv'he` C
 'D' -> Letter `ha` Upper `hv'he` D
 'E' -> Letter `ha` Upper `hv'he` E
 'F' -> Letter `ha` Upper `hv'he` F
 'G' -> Letter `ha` Upper `hv'he` G
 'H' -> Letter `ha` Upper `hv'he` H
 'I' -> Letter `ha` Upper `hv'he` I
 'J' -> Letter `ha` Upper `hv'he` J
 'K' -> Letter `ha` Upper `hv'he` K
 'L' -> Letter `ha` Upper `hv'he` L
 'M' -> Letter `ha` Upper `hv'he` M
 'N' -> Letter `ha` Upper `hv'he` N
 'O' -> Letter `ha` Upper `hv'he` O
 'P' -> Letter `ha` Upper `hv'he` P
 'Q' -> Letter `ha` Upper `hv'he` Q
 'R' -> Letter `ha` Upper `hv'he` R
 'S' -> Letter `ha` Upper `hv'he` S
 'T' -> Letter `ha` Upper `hv'he` T
 'U' -> Letter `ha` Upper `hv'he` U
 'V' -> Letter `ha` Upper `hv'he` V
 'W' -> Letter `ha` Upper `hv'he` W
 'X' -> Letter `ha` Upper `hv'he` X
 'Y' -> Letter `ha` Upper `hv'he` Y
 'Z' -> Letter `ha` Upper `hv'he` Z
 'a' -> Letter `ha` Lower `hv'he` A
 'b' -> Letter `ha` Lower `hv'he` B
 'c' -> Letter `ha` Lower `hv'he` C
 'd' -> Letter `ha` Lower `hv'he` D
 'e' -> Letter `ha` Lower `hv'he` E
 'f' -> Letter `ha` Lower `hv'he` F
 'g' -> Letter `ha` Lower `hv'he` G
 'h' -> Letter `ha` Lower `hv'he` H
 'i' -> Letter `ha` Lower `hv'he` I
 'j' -> Letter `ha` Lower `hv'he` J
 'k' -> Letter `ha` Lower `hv'he` K
 'l' -> Letter `ha` Lower `hv'he` L
 'm' -> Letter `ha` Lower `hv'he` M
 'n' -> Letter `ha` Lower `hv'he` N
 'o' -> Letter `ha` Lower `hv'he` O
 'p' -> Letter `ha` Lower `hv'he` P
 'q' -> Letter `ha` Lower `hv'he` Q
 'r' -> Letter `ha` Lower `hv'he` R
 's' -> Letter `ha` Lower `hv'he` S
 't' -> Letter `ha` Lower `hv'he` T
 'u' -> Letter `ha` Lower `hv'he` U
 'v' -> Letter `ha` Lower `hv'he` V
 'w' -> Letter `ha` Lower `hv'he` W
 'x' -> Letter `ha` Lower `hv'he` X
 'y' -> Letter `ha` Lower `hv'he` Y
 'z' -> Letter `ha` Lower `hv'he` Z
 '0' -> Digit `hv'he` Zero
 '1' -> Digit `hv'he` One
 '2' -> Digit `hv'he` Two
 '3' -> Digit `hv'he` Three
 '4' -> Digit `hv'he` Four
 '5' -> Digit `hv'he` Five
 '6' -> Digit `hv'he` Six
 '7' -> Digit `hv'he` Seven
 '8' -> Digit `hv'he` Eight
 '9' -> Digit `hv'he` Nine
 _ -> error "Not a Glyph!"

char_to_ascii = \case
 '\BS' -> Caret `hv'he` (Back `ha` Space)
 '\HT' -> Caret `hv'he` Tab
 '\LF' -> Caret `hv'he` Newline
 '\ESC' -> Caret `hv'he` Escape
 ' ' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Space
 '\DEL' -> Caret `hv'he` Delete
 '/' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` (Back `ha` Slash)
 '\\' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Slash
 '(' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv'he` Round
 ')' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv'he` Round
 '{' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv'he` Curly
 '}' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv'he` Curly
 '<' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv'he` Angle
 '>' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv'he` Angle
 '[' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `hv'he` Square
 ']' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `hv'he` Square
 '"' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Doublequote
 '\'' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Singlequote
 '.' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Period
 ',' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Comma
 ';' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Semicolon
 ':' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Colon
 '!' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Exclam
 '?' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Question
 '#' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Hash
 '$' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Dollar
 '%' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Percent
 '&' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Ampersand
 '*' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Asterisk
 '+' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Plus
 '-' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Hyphen
 '=' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Equality
 '@' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` At
 '^' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Circumflex
 '_' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Underscore
 '`' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Grave
 '|' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Bar
 '~' -> Glyph `ha` Symbol `ha` Punctuate `hv'he` Tilde
 'A' -> Glyph `ha` Letter `ha` Upper `hv'he` A
 'B' -> Glyph `ha` Letter `ha` Upper `hv'he` B
 'C' -> Glyph `ha` Letter `ha` Upper `hv'he` C
 'D' -> Glyph `ha` Letter `ha` Upper `hv'he` D
 'E' -> Glyph `ha` Letter `ha` Upper `hv'he` E
 'F' -> Glyph `ha` Letter `ha` Upper `hv'he` F
 'G' -> Glyph `ha` Letter `ha` Upper `hv'he` G
 'H' -> Glyph `ha` Letter `ha` Upper `hv'he` H
 'I' -> Glyph `ha` Letter `ha` Upper `hv'he` I
 'J' -> Glyph `ha` Letter `ha` Upper `hv'he` J
 'K' -> Glyph `ha` Letter `ha` Upper `hv'he` K
 'L' -> Glyph `ha` Letter `ha` Upper `hv'he` L
 'M' -> Glyph `ha` Letter `ha` Upper `hv'he` M
 'N' -> Glyph `ha` Letter `ha` Upper `hv'he` N
 'O' -> Glyph `ha` Letter `ha` Upper `hv'he` O
 'P' -> Glyph `ha` Letter `ha` Upper `hv'he` P
 'Q' -> Glyph `ha` Letter `ha` Upper `hv'he` Q
 'R' -> Glyph `ha` Letter `ha` Upper `hv'he` R
 'S' -> Glyph `ha` Letter `ha` Upper `hv'he` S
 'T' -> Glyph `ha` Letter `ha` Upper `hv'he` T
 'U' -> Glyph `ha` Letter `ha` Upper `hv'he` U
 'V' -> Glyph `ha` Letter `ha` Upper `hv'he` V
 'W' -> Glyph `ha` Letter `ha` Upper `hv'he` W
 'X' -> Glyph `ha` Letter `ha` Upper `hv'he` X
 'Y' -> Glyph `ha` Letter `ha` Upper `hv'he` Y
 'Z' -> Glyph `ha` Letter `ha` Upper `hv'he` Z
 'a' -> Glyph `ha` Letter `ha` Lower `hv'he` A
 'b' -> Glyph `ha` Letter `ha` Lower `hv'he` B
 'c' -> Glyph `ha` Letter `ha` Lower `hv'he` C
 'd' -> Glyph `ha` Letter `ha` Lower `hv'he` D
 'e' -> Glyph `ha` Letter `ha` Lower `hv'he` E
 'f' -> Glyph `ha` Letter `ha` Lower `hv'he` F
 'g' -> Glyph `ha` Letter `ha` Lower `hv'he` G
 'h' -> Glyph `ha` Letter `ha` Lower `hv'he` H
 'i' -> Glyph `ha` Letter `ha` Lower `hv'he` I
 'j' -> Glyph `ha` Letter `ha` Lower `hv'he` J
 'k' -> Glyph `ha` Letter `ha` Lower `hv'he` K
 'l' -> Glyph `ha` Letter `ha` Lower `hv'he` L
 'm' -> Glyph `ha` Letter `ha` Lower `hv'he` M
 'n' -> Glyph `ha` Letter `ha` Lower `hv'he` N
 'o' -> Glyph `ha` Letter `ha` Lower `hv'he` O
 'p' -> Glyph `ha` Letter `ha` Lower `hv'he` P
 'q' -> Glyph `ha` Letter `ha` Lower `hv'he` Q
 'r' -> Glyph `ha` Letter `ha` Lower `hv'he` R
 's' -> Glyph `ha` Letter `ha` Lower `hv'he` S
 't' -> Glyph `ha` Letter `ha` Lower `hv'he` T
 'u' -> Glyph `ha` Letter `ha` Lower `hv'he` U
 'v' -> Glyph `ha` Letter `ha` Lower `hv'he` V
 'w' -> Glyph `ha` Letter `ha` Lower `hv'he` W
 'x' -> Glyph `ha` Letter `ha` Lower `hv'he` X
 'y' -> Glyph `ha` Letter `ha` Lower `hv'he` Y
 'z' -> Glyph `ha` Letter `ha` Lower `hv'he` Z
 '0' -> Glyph `ha` Digit `hv'he` Zero
 '1' -> Glyph `ha` Digit `hv'he` One
 '2' -> Glyph `ha` Digit `hv'he` Two
 '3' -> Glyph `ha` Digit `hv'he` Three
 '4' -> Glyph `ha` Digit `hv'he` Four
 '5' -> Glyph `ha` Digit `hv'he` Five
 '6' -> Glyph `ha` Digit `hv'he` Six
 '7' -> Glyph `ha` Digit `hv'he` Seven
 '8' -> Glyph `ha` Digit `hv'he` Eight
 '9' -> Glyph `ha` Digit `hv'he` Nine
 _ -> error "Not ASCII!"
