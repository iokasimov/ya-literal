module Ya.Literal.Utils where

import Ya
import Ya.ASCII

import "base" Data.Char (Char)
import "base" GHC.Err (error)

char_to_letter :: Char -> Letter
char_to_letter = \case
 'A' -> Upper `hc'st` A
 'B' -> Upper `hc'st` B
 'C' -> Upper `hc'st` C
 'D' -> Upper `hc'st` D
 'E' -> Upper `hc'st` E
 'F' -> Upper `hc'st` F
 'G' -> Upper `hc'st` G
 'H' -> Upper `hc'st` H
 'I' -> Upper `hc'st` I
 'J' -> Upper `hc'st` J
 'K' -> Upper `hc'st` K
 'L' -> Upper `hc'st` L
 'M' -> Upper `hc'st` M
 'N' -> Upper `hc'st` N
 'O' -> Upper `hc'st` O
 'P' -> Upper `hc'st` P
 'Q' -> Upper `hc'st` Q
 'R' -> Upper `hc'st` R
 'S' -> Upper `hc'st` S
 'T' -> Upper `hc'st` T
 'U' -> Upper `hc'st` U
 'V' -> Upper `hc'st` V
 'W' -> Upper `hc'st` W
 'X' -> Upper `hc'st` X
 'Y' -> Upper `hc'st` Y
 'Z' -> Upper `hc'st` Z
 'a' -> Lower `hc'st` A
 'b' -> Lower `hc'st` B
 'c' -> Lower `hc'st` C
 'd' -> Lower `hc'st` D
 'e' -> Lower `hc'st` E
 'f' -> Lower `hc'st` F
 'g' -> Lower `hc'st` G
 'h' -> Lower `hc'st` H
 'i' -> Lower `hc'st` I
 'j' -> Lower `hc'st` J
 'k' -> Lower `hc'st` K
 'l' -> Lower `hc'st` L
 'm' -> Lower `hc'st` M
 'n' -> Lower `hc'st` N
 'o' -> Lower `hc'st` O
 'p' -> Lower `hc'st` P
 'q' -> Lower `hc'st` Q
 'r' -> Lower `hc'st` R
 's' -> Lower `hc'st` S
 't' -> Lower `hc'st` T
 'u' -> Lower `hc'st` U
 'v' -> Lower `hc'st` V
 'w' -> Lower `hc'st` W
 'x' -> Lower `hc'st` X
 x -> error ('<' : x : "> is not a latin character!")

caret_to_char :: Caret -> Char
caret_to_char = is `hu` '\HT' `has` is `hu` '\LF' `has` is `hu` '\ESC' `has` is `hu` '\BS' `has` is `hu` '\DEL'

bracket_to_char :: Bracket -> Char
bracket_to_char = is `hu` '(' `has` is `hu` '{' `has` is `hu` '<' `has` is `hu` '['
              `has_` is `hu` ')' `has` is `hu` '}' `has` is `hu` '>' `has` is `hu` ']'

punctuation_to_char :: Punctuate -> Char
punctuation_to_char = is `hu` '\"' `has` is `hu` '\'' `has` is `hu` '#' `has` is `hu` '=' `has` is `hu` '-' `has` is `hu` '@'
 `has` is `hu` '^' `has` is `hu` '_' `has` is `hu` '`' `has` is `hu` '|' `has` is `hu` '~'
 `has` is `hu` '+' `has` is `hu` '*' `has` is `hu` '%' `has` is `hu` '&' `has` is `hu` '$' `has` is `hu` '\\' `has` is `hu` '/'
 `has` is `hu` '.' `has` is `hu` ',' `has` is `hu` ';' `has` is `hu` ':' `has` is `hu` '!' `has` is `hu` '?' `has` is `hu` ' '

upper_latin_to_char :: Latin -> Char
upper_latin_to_char = is `hu` 'A' `has` is `hu` 'B' `has` is `hu` 'C' `has` is `hu` 'D' `has` is `hu` 'E' `has` is `hu` 'F'
 `has` is `hu` 'G' `has` is `hu` 'H' `has` is `hu` 'I' `has` is `hu` 'J' `has` is `hu` 'K' `has` is `hu` 'L'
 `has` is `hu` 'M' `has` is `hu` 'N' `has` is `hu` 'O' `has` is `hu` 'P' `has` is `hu` 'Q' `has` is `hu` 'R'
 `has` is `hu` 'S' `has` is `hu` 'T' `has` is `hu` 'U' `has` is `hu` 'V' `has` is `hu` 'W' `has` is `hu` 'X'
 `has` is `hu` 'Y' `has` is `hu` 'Z'

lower_latin_to_char :: Latin -> Char
lower_latin_to_char = is `hu` 'a' `has` is `hu` 'b' `has` is `hu` 'c' `has` is `hu` 'd' `has` is `hu` 'e' `has` is `hu` 'f'
 `has` is `hu` 'g' `has` is `hu` 'h' `has` is `hu` 'i' `has` is `hu` 'j' `has` is `hu` 'k' `has` is `hu` 'l'
 `has` is `hu` 'm' `has` is `hu` 'n' `has` is `hu` 'o' `has` is `hu` 'p' `has` is `hu` 'q' `has` is `hu` 'r'
 `has` is `hu` 's' `has` is `hu` 't' `has` is `hu` 'u' `has` is `hu` 'v' `has` is `hu` 'w' `has` is `hu` 'x'
 `has` is `hu` 'y' `has` is `hu` 'z'

digit_to_char :: Digit -> Char
digit_to_char = is `hu` '0' `has` is `hu` '1' `has` is `hu` '2' `has` is `hu` '3' `has` is `hu` '4' `has` is `hu` '5' `has` is `hu` '6' `has` is `hu` '7' `has` is `hu` '8' `has` is `hu` '9'

ascii_to_char :: ASCII -> Char
ascii_to_char = is
 `hc_` lower_latin_to_char
  `has` upper_latin_to_char
 `has_` digit_to_char `ha` is
 `has_` bracket_to_char `ha` is
  `has` punctuation_to_char `ha` is
 `has_` caret_to_char `ha` is

glyph_to_ascii = \case
 ' ' -> Symbol `ha` Punctuate `hc'st` Space
 '/' -> Symbol `ha` Punctuate `hc'st` (Back `ha` Slash)
 '\\' -> Symbol `ha` Punctuate `hc'st` Slash
 '(' -> Symbol `ha` Bracket `ha` Opened `hc'st` Round
 ')' -> Symbol `ha` Bracket `ha` Closed `hc'st` Round
 '{' -> Symbol `ha` Bracket `ha` Opened `hc'st` Curly
 '}' -> Symbol `ha` Bracket `ha` Closed `hc'st` Curly
 '<' -> Symbol `ha` Bracket `ha` Opened `hc'st` Angle
 '>' -> Symbol `ha` Bracket `ha` Closed `hc'st` Angle
 '[' -> Symbol `ha` Bracket `ha` Opened `hc'st` Square
 ']' -> Symbol `ha` Bracket `ha` Closed `hc'st` Square
 '"' -> Symbol `ha` Punctuate `hc'st` Doublequote
 '\'' -> Symbol `ha` Punctuate `hc'st` Singlequote
 '.' -> Symbol `ha` Punctuate `hc'st` Period
 ',' -> Symbol `ha` Punctuate `hc'st` Comma
 ';' -> Symbol `ha` Punctuate `hc'st` Semicolon
 ':' -> Symbol `ha` Punctuate `hc'st` Colon
 '!' -> Symbol `ha` Punctuate `hc'st` Exclam
 '?' -> Symbol `ha` Punctuate `hc'st` Question
 '#' -> Symbol `ha` Punctuate `hc'st` Hash
 '$' -> Symbol `ha` Punctuate `hc'st` Dollar
 '%' -> Symbol `ha` Punctuate `hc'st` Percent
 '&' -> Symbol `ha` Punctuate `hc'st` Ampersand
 '*' -> Symbol `ha` Punctuate `hc'st` Asterisk
 '+' -> Symbol `ha` Punctuate `hc'st` Plus
 '-' -> Symbol `ha` Punctuate `hc'st` Hyphen
 '=' -> Symbol `ha` Punctuate `hc'st` Equality
 '@' -> Symbol `ha` Punctuate `hc'st` At
 '^' -> Symbol `ha` Punctuate `hc'st` Circumflex
 '_' -> Symbol `ha` Punctuate `hc'st` Underscore
 '`' -> Symbol `ha` Punctuate `hc'st` Grave
 '|' -> Symbol `ha` Punctuate `hc'st` Bar
 '~' -> Symbol `ha` Punctuate `hc'st` Tilde
 'A' -> Letter `ha` Upper `hc'st` A
 'B' -> Letter `ha` Upper `hc'st` B
 'C' -> Letter `ha` Upper `hc'st` C
 'D' -> Letter `ha` Upper `hc'st` D
 'E' -> Letter `ha` Upper `hc'st` E
 'F' -> Letter `ha` Upper `hc'st` F
 'G' -> Letter `ha` Upper `hc'st` G
 'H' -> Letter `ha` Upper `hc'st` H
 'I' -> Letter `ha` Upper `hc'st` I
 'J' -> Letter `ha` Upper `hc'st` J
 'K' -> Letter `ha` Upper `hc'st` K
 'L' -> Letter `ha` Upper `hc'st` L
 'M' -> Letter `ha` Upper `hc'st` M
 'N' -> Letter `ha` Upper `hc'st` N
 'O' -> Letter `ha` Upper `hc'st` O
 'P' -> Letter `ha` Upper `hc'st` P
 'Q' -> Letter `ha` Upper `hc'st` Q
 'R' -> Letter `ha` Upper `hc'st` R
 'S' -> Letter `ha` Upper `hc'st` S
 'T' -> Letter `ha` Upper `hc'st` T
 'U' -> Letter `ha` Upper `hc'st` U
 'V' -> Letter `ha` Upper `hc'st` V
 'W' -> Letter `ha` Upper `hc'st` W
 'X' -> Letter `ha` Upper `hc'st` X
 'Y' -> Letter `ha` Upper `hc'st` Y
 'Z' -> Letter `ha` Upper `hc'st` Z
 'a' -> Letter `ha` Lower `hc'st` A
 'b' -> Letter `ha` Lower `hc'st` B
 'c' -> Letter `ha` Lower `hc'st` C
 'd' -> Letter `ha` Lower `hc'st` D
 'e' -> Letter `ha` Lower `hc'st` E
 'f' -> Letter `ha` Lower `hc'st` F
 'g' -> Letter `ha` Lower `hc'st` G
 'h' -> Letter `ha` Lower `hc'st` H
 'i' -> Letter `ha` Lower `hc'st` I
 'j' -> Letter `ha` Lower `hc'st` J
 'k' -> Letter `ha` Lower `hc'st` K
 'l' -> Letter `ha` Lower `hc'st` L
 'm' -> Letter `ha` Lower `hc'st` M
 'n' -> Letter `ha` Lower `hc'st` N
 'o' -> Letter `ha` Lower `hc'st` O
 'p' -> Letter `ha` Lower `hc'st` P
 'q' -> Letter `ha` Lower `hc'st` Q
 'r' -> Letter `ha` Lower `hc'st` R
 's' -> Letter `ha` Lower `hc'st` S
 't' -> Letter `ha` Lower `hc'st` T
 'u' -> Letter `ha` Lower `hc'st` U
 'v' -> Letter `ha` Lower `hc'st` V
 'w' -> Letter `ha` Lower `hc'st` W
 'x' -> Letter `ha` Lower `hc'st` X
 'y' -> Letter `ha` Lower `hc'st` Y
 'z' -> Letter `ha` Lower `hc'st` Z
 '0' -> Digit `hc'st` Zero
 '1' -> Digit `hc'st` One
 '2' -> Digit `hc'st` Two
 '3' -> Digit `hc'st` Three
 '4' -> Digit `hc'st` Four
 '5' -> Digit `hc'st` Five
 '6' -> Digit `hc'st` Six
 '7' -> Digit `hc'st` Seven
 '8' -> Digit `hc'st` Eight
 '9' -> Digit `hc'st` Nine
 _ -> error "Not a Glyph!"

char_to_ascii = \case
 '\BS' -> Exist `ha` Caret `hc'st` (Back `ha` Space)
 '\HT' -> Exist `ha` Caret `hc'st` Tab
 '\LF' -> Exist `ha` Caret `hc'st` Newline
 '\ESC' -> Exist `ha` Caret `hc'st` Escape
 ' ' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Space
 '\DEL' -> Exist `ha` Caret `hc'st` Delete
 '/' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` (Back `ha` Slash)
 '\\' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Slash
 '(' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hc'st` Round
 ')' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hc'st` Round
 '{' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hc'st` Curly
 '}' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hc'st` Curly
 '<' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hc'st` Angle
 '>' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hc'st` Angle
 '[' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `hc'st` Square
 ']' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `hc'st` Square
 '"' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Doublequote
 '\'' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Singlequote
 '.' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Period
 ',' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Comma
 ';' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Semicolon
 ':' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Colon
 '!' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Exclam
 '?' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Question
 '#' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Hash
 '$' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Dollar
 '%' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Percent
 '&' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Ampersand
 '*' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Asterisk
 '+' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Plus
 '-' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Hyphen
 '=' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Equality
 '@' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` At
 '^' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Circumflex
 '_' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Underscore
 '`' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Grave
 '|' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Bar
 '~' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `hc'st` Tilde
 'A' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` A
 'B' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` B
 'C' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` C
 'D' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` D
 'E' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` E
 'F' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` F
 'G' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` G
 'H' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` H
 'I' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` I
 'J' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` J
 'K' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` K
 'L' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` L
 'M' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` M
 'N' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` N
 'O' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` O
 'P' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` P
 'Q' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` Q
 'R' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` R
 'S' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` S
 'T' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` T
 'U' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` U
 'V' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` V
 'W' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` W
 'X' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` X
 'Y' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` Y
 'Z' -> Exist `ha` Glyph `ha` Letter `ha` Upper `hc'st` Z
 'a' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` A
 'b' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` B
 'c' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` C
 'd' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` D
 'e' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` E
 'f' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` F
 'g' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` G
 'h' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` H
 'i' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` I
 'j' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` J
 'k' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` K
 'l' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` L
 'm' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` M
 'n' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` N
 'o' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` O
 'p' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` P
 'q' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` Q
 'r' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` R
 's' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` S
 't' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` T
 'u' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` U
 'v' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` V
 'w' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` W
 'x' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` X
 'y' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` Y
 'z' -> Exist `ha` Glyph `ha` Letter `ha` Lower `hc'st` Z
 '0' -> Exist `ha` Glyph `ha` Digit `hc'st` Zero
 '1' -> Exist `ha` Glyph `ha` Digit `hc'st` One
 '2' -> Exist `ha` Glyph `ha` Digit `hc'st` Two
 '3' -> Exist `ha` Glyph `ha` Digit `hc'st` Three
 '4' -> Exist `ha` Glyph `ha` Digit `hc'st` Four
 '5' -> Exist `ha` Glyph `ha` Digit `hc'st` Five
 '6' -> Exist `ha` Glyph `ha` Digit `hc'st` Six
 '7' -> Exist `ha` Glyph `ha` Digit `hc'st` Seven
 '8' -> Exist `ha` Glyph `ha` Digit `hc'st` Eight
 '9' -> Exist `ha` Glyph `ha` Digit `hc'st` Nine
 _ -> supertype Empty
