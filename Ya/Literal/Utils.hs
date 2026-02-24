module Ya.Literal.Utils where

import Ya
import Ya.ASCII

import "base" Data.Char (Char)
import "base" GHC.Err (error)

char_to_letter :: Char -> Letter
char_to_letter = \case
 'A' -> Upper `har'st` A
 'B' -> Upper `har'st` B
 'C' -> Upper `har'st` C
 'D' -> Upper `har'st` D
 'E' -> Upper `har'st` E
 'F' -> Upper `har'st` F
 'G' -> Upper `har'st` G
 'H' -> Upper `har'st` H
 'I' -> Upper `har'st` I
 'J' -> Upper `har'st` J
 'K' -> Upper `har'st` K
 'L' -> Upper `har'st` L
 'M' -> Upper `har'st` M
 'N' -> Upper `har'st` N
 'O' -> Upper `har'st` O
 'P' -> Upper `har'st` P
 'Q' -> Upper `har'st` Q
 'R' -> Upper `har'st` R
 'S' -> Upper `har'st` S
 'T' -> Upper `har'st` T
 'U' -> Upper `har'st` U
 'V' -> Upper `har'st` V
 'W' -> Upper `har'st` W
 'X' -> Upper `har'st` X
 'Y' -> Upper `har'st` Y
 'Z' -> Upper `har'st` Z
 'a' -> Lower `har'st` A
 'b' -> Lower `har'st` B
 'c' -> Lower `har'st` C
 'd' -> Lower `har'st` D
 'e' -> Lower `har'st` E
 'f' -> Lower `har'st` F
 'g' -> Lower `har'st` G
 'h' -> Lower `har'st` H
 'i' -> Lower `har'st` I
 'j' -> Lower `har'st` J
 'k' -> Lower `har'st` K
 'l' -> Lower `har'st` L
 'm' -> Lower `har'st` M
 'n' -> Lower `har'st` N
 'o' -> Lower `har'st` O
 'p' -> Lower `har'st` P
 'q' -> Lower `har'st` Q
 'r' -> Lower `har'st` R
 's' -> Lower `har'st` S
 't' -> Lower `har'st` T
 'u' -> Lower `har'st` U
 'v' -> Lower `har'st` V
 'w' -> Lower `har'st` W
 'x' -> Lower `har'st` X
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
 `har` lower_latin_to_char
  `has` upper_latin_to_char
 `has_` digit_to_char `ha` is
 `has_` bracket_to_char `ha` is
  `has` punctuation_to_char `ha` is
 `has_` caret_to_char `ha` is

glyph_to_ascii = \case
 ' ' -> Symbol `ha` Punctuate `har'st` Space
 '/' -> Symbol `ha` Punctuate `har'st` (Back `ha` Slash)
 '\\' -> Symbol `ha` Punctuate `har'st` Slash
 '(' -> Symbol `ha` Bracket `ha` Opened `har'st` Round
 ')' -> Symbol `ha` Bracket `ha` Closed `har'st` Round
 '{' -> Symbol `ha` Bracket `ha` Opened `har'st` Curly
 '}' -> Symbol `ha` Bracket `ha` Closed `har'st` Curly
 '<' -> Symbol `ha` Bracket `ha` Opened `har'st` Angle
 '>' -> Symbol `ha` Bracket `ha` Closed `har'st` Angle
 '[' -> Symbol `ha` Bracket `ha` Opened `har'st` Square
 ']' -> Symbol `ha` Bracket `ha` Closed `har'st` Square
 '"' -> Symbol `ha` Punctuate `har'st` Doublequote
 '\'' -> Symbol `ha` Punctuate `har'st` Singlequote
 '.' -> Symbol `ha` Punctuate `har'st` Period
 ',' -> Symbol `ha` Punctuate `har'st` Comma
 ';' -> Symbol `ha` Punctuate `har'st` Semicolon
 ':' -> Symbol `ha` Punctuate `har'st` Colon
 '!' -> Symbol `ha` Punctuate `har'st` Exclam
 '?' -> Symbol `ha` Punctuate `har'st` Question
 '#' -> Symbol `ha` Punctuate `har'st` Hash
 '$' -> Symbol `ha` Punctuate `har'st` Dollar
 '%' -> Symbol `ha` Punctuate `har'st` Percent
 '&' -> Symbol `ha` Punctuate `har'st` Ampersand
 '*' -> Symbol `ha` Punctuate `har'st` Asterisk
 '+' -> Symbol `ha` Punctuate `har'st` Plus
 '-' -> Symbol `ha` Punctuate `har'st` Hyphen
 '=' -> Symbol `ha` Punctuate `har'st` Equality
 '@' -> Symbol `ha` Punctuate `har'st` At
 '^' -> Symbol `ha` Punctuate `har'st` Circumflex
 '_' -> Symbol `ha` Punctuate `har'st` Underscore
 '`' -> Symbol `ha` Punctuate `har'st` Grave
 '|' -> Symbol `ha` Punctuate `har'st` Bar
 '~' -> Symbol `ha` Punctuate `har'st` Tilde
 'A' -> Letter `ha` Upper `har'st` A
 'B' -> Letter `ha` Upper `har'st` B
 'C' -> Letter `ha` Upper `har'st` C
 'D' -> Letter `ha` Upper `har'st` D
 'E' -> Letter `ha` Upper `har'st` E
 'F' -> Letter `ha` Upper `har'st` F
 'G' -> Letter `ha` Upper `har'st` G
 'H' -> Letter `ha` Upper `har'st` H
 'I' -> Letter `ha` Upper `har'st` I
 'J' -> Letter `ha` Upper `har'st` J
 'K' -> Letter `ha` Upper `har'st` K
 'L' -> Letter `ha` Upper `har'st` L
 'M' -> Letter `ha` Upper `har'st` M
 'N' -> Letter `ha` Upper `har'st` N
 'O' -> Letter `ha` Upper `har'st` O
 'P' -> Letter `ha` Upper `har'st` P
 'Q' -> Letter `ha` Upper `har'st` Q
 'R' -> Letter `ha` Upper `har'st` R
 'S' -> Letter `ha` Upper `har'st` S
 'T' -> Letter `ha` Upper `har'st` T
 'U' -> Letter `ha` Upper `har'st` U
 'V' -> Letter `ha` Upper `har'st` V
 'W' -> Letter `ha` Upper `har'st` W
 'X' -> Letter `ha` Upper `har'st` X
 'Y' -> Letter `ha` Upper `har'st` Y
 'Z' -> Letter `ha` Upper `har'st` Z
 'a' -> Letter `ha` Lower `har'st` A
 'b' -> Letter `ha` Lower `har'st` B
 'c' -> Letter `ha` Lower `har'st` C
 'd' -> Letter `ha` Lower `har'st` D
 'e' -> Letter `ha` Lower `har'st` E
 'f' -> Letter `ha` Lower `har'st` F
 'g' -> Letter `ha` Lower `har'st` G
 'h' -> Letter `ha` Lower `har'st` H
 'i' -> Letter `ha` Lower `har'st` I
 'j' -> Letter `ha` Lower `har'st` J
 'k' -> Letter `ha` Lower `har'st` K
 'l' -> Letter `ha` Lower `har'st` L
 'm' -> Letter `ha` Lower `har'st` M
 'n' -> Letter `ha` Lower `har'st` N
 'o' -> Letter `ha` Lower `har'st` O
 'p' -> Letter `ha` Lower `har'st` P
 'q' -> Letter `ha` Lower `har'st` Q
 'r' -> Letter `ha` Lower `har'st` R
 's' -> Letter `ha` Lower `har'st` S
 't' -> Letter `ha` Lower `har'st` T
 'u' -> Letter `ha` Lower `har'st` U
 'v' -> Letter `ha` Lower `har'st` V
 'w' -> Letter `ha` Lower `har'st` W
 'x' -> Letter `ha` Lower `har'st` X
 'y' -> Letter `ha` Lower `har'st` Y
 'z' -> Letter `ha` Lower `har'st` Z
 '0' -> Digit `har'st` Zero
 '1' -> Digit `har'st` One
 '2' -> Digit `har'st` Two
 '3' -> Digit `har'st` Three
 '4' -> Digit `har'st` Four
 '5' -> Digit `har'st` Five
 '6' -> Digit `har'st` Six
 '7' -> Digit `har'st` Seven
 '8' -> Digit `har'st` Eight
 '9' -> Digit `har'st` Nine
 _ -> error "Not a Glyph!"

char_to_ascii = \case
 '\BS' -> Exist `ha` Caret `har'st` (Back `ha` Space)
 '\HT' -> Exist `ha` Caret `har'st` Tab
 '\LF' -> Exist `ha` Caret `har'st` Newline
 '\ESC' -> Exist `ha` Caret `har'st` Escape
 ' ' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Space
 '\DEL' -> Exist `ha` Caret `har'st` Delete
 '/' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` (Back `ha` Slash)
 '\\' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Slash
 '(' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `har'st` Round
 ')' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `har'st` Round
 '{' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `har'st` Curly
 '}' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `har'st` Curly
 '<' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `har'st` Angle
 '>' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `har'st` Angle
 '[' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `har'st` Square
 ']' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `har'st` Square
 '"' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Doublequote
 '\'' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Singlequote
 '.' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Period
 ',' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Comma
 ';' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Semicolon
 ':' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Colon
 '!' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Exclam
 '?' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Question
 '#' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Hash
 '$' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Dollar
 '%' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Percent
 '&' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Ampersand
 '*' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Asterisk
 '+' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Plus
 '-' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Hyphen
 '=' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Equality
 '@' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` At
 '^' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Circumflex
 '_' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Underscore
 '`' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Grave
 '|' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Bar
 '~' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `har'st` Tilde
 'A' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` A
 'B' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` B
 'C' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` C
 'D' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` D
 'E' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` E
 'F' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` F
 'G' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` G
 'H' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` H
 'I' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` I
 'J' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` J
 'K' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` K
 'L' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` L
 'M' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` M
 'N' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` N
 'O' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` O
 'P' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` P
 'Q' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` Q
 'R' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` R
 'S' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` S
 'T' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` T
 'U' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` U
 'V' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` V
 'W' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` W
 'X' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` X
 'Y' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` Y
 'Z' -> Exist `ha` Glyph `ha` Letter `ha` Upper `har'st` Z
 'a' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` A
 'b' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` B
 'c' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` C
 'd' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` D
 'e' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` E
 'f' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` F
 'g' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` G
 'h' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` H
 'i' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` I
 'j' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` J
 'k' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` K
 'l' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` L
 'm' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` M
 'n' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` N
 'o' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` O
 'p' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` P
 'q' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` Q
 'r' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` R
 's' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` S
 't' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` T
 'u' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` U
 'v' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` V
 'w' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` W
 'x' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` X
 'y' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` Y
 'z' -> Exist `ha` Glyph `ha` Letter `ha` Lower `har'st` Z
 '0' -> Exist `ha` Glyph `ha` Digit `har'st` Zero
 '1' -> Exist `ha` Glyph `ha` Digit `har'st` One
 '2' -> Exist `ha` Glyph `ha` Digit `har'st` Two
 '3' -> Exist `ha` Glyph `ha` Digit `har'st` Three
 '4' -> Exist `ha` Glyph `ha` Digit `har'st` Four
 '5' -> Exist `ha` Glyph `ha` Digit `har'st` Five
 '6' -> Exist `ha` Glyph `ha` Digit `har'st` Six
 '7' -> Exist `ha` Glyph `ha` Digit `har'st` Seven
 '8' -> Exist `ha` Glyph `ha` Digit `har'st` Eight
 '9' -> Exist `ha` Glyph `ha` Digit `har'st` Nine
 _ -> supertype Empty
