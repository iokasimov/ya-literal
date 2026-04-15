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
caret_to_char = is `ho'ut` '\HT' `has` is `ho'ut` '\LF' `has` is `ho'ut` '\ESC' `has` is `ho'ut` '\BS' `has` is `ho'ut` '\DEL'

bracket_to_char :: Bracket -> Char
bracket_to_char = is `ho'ut` '(' `has` is `ho'ut` '{' `has` is `ho'ut` '<' `has` is `ho'ut` '['
              `has_` is `ho'ut` ')' `has` is `ho'ut` '}' `has` is `ho'ut` '>' `has` is `ho'ut` ']'

punctuation_to_char :: Punctuate -> Char
punctuation_to_char = is `ho'ut` '\"' `has` is `ho'ut` '\'' `has` is `ho'ut` '#' `has` is `ho'ut` '=' `has` is `ho'ut` '-' `has` is `ho'ut` '@'
 `has` is `ho'ut` '^' `has` is `ho'ut` '_' `has` is `ho'ut` '`' `has` is `ho'ut` '|' `has` is `ho'ut` '~'
 `has` is `ho'ut` '+' `has` is `ho'ut` '*' `has` is `ho'ut` '%' `has` is `ho'ut` '&' `has` is `ho'ut` '$' `has` is `ho'ut` '\\' `has` is `ho'ut` '/'
 `has` is `ho'ut` '.' `has` is `ho'ut` ',' `has` is `ho'ut` ';' `has` is `ho'ut` ':' `has` is `ho'ut` '!' `has` is `ho'ut` '?' `has` is `ho'ut` ' '

upper_latin_to_char :: Latin -> Char
upper_latin_to_char = is `ho'ut` 'A' `has` is `ho'ut` 'B' `has` is `ho'ut` 'C' `has` is `ho'ut` 'D' `has` is `ho'ut` 'E' `has` is `ho'ut` 'F'
 `has` is `ho'ut` 'G' `has` is `ho'ut` 'H' `has` is `ho'ut` 'I' `has` is `ho'ut` 'J' `has` is `ho'ut` 'K' `has` is `ho'ut` 'L'
 `has` is `ho'ut` 'M' `has` is `ho'ut` 'N' `has` is `ho'ut` 'O' `has` is `ho'ut` 'P' `has` is `ho'ut` 'Q' `has` is `ho'ut` 'R'
 `has` is `ho'ut` 'S' `has` is `ho'ut` 'T' `has` is `ho'ut` 'U' `has` is `ho'ut` 'V' `has` is `ho'ut` 'W' `has` is `ho'ut` 'X'
 `has` is `ho'ut` 'Y' `has` is `ho'ut` 'Z'

lower_latin_to_char :: Latin -> Char
lower_latin_to_char = is `ho'ut` 'a' `has` is `ho'ut` 'b' `has` is `ho'ut` 'c' `has` is `ho'ut` 'd' `has` is `ho'ut` 'e' `has` is `ho'ut` 'f'
 `has` is `ho'ut` 'g' `has` is `ho'ut` 'h' `has` is `ho'ut` 'i' `has` is `ho'ut` 'j' `has` is `ho'ut` 'k' `has` is `ho'ut` 'l'
 `has` is `ho'ut` 'm' `has` is `ho'ut` 'n' `has` is `ho'ut` 'o' `has` is `ho'ut` 'p' `has` is `ho'ut` 'q' `has` is `ho'ut` 'r'
 `has` is `ho'ut` 's' `has` is `ho'ut` 't' `has` is `ho'ut` 'u' `has` is `ho'ut` 'v' `has` is `ho'ut` 'w' `has` is `ho'ut` 'x'
 `has` is `ho'ut` 'y' `has` is `ho'ut` 'z'

digit_to_char :: Digit -> Char
digit_to_char = is `ho'ut` '0' `has` is `ho'ut` '1' `has` is `ho'ut` '2' `has` is `ho'ut` '3' `has` is `ho'ut` '4' `has` is `ho'ut` '5' `has` is `ho'ut` '6' `has` is `ho'ut` '7' `has` is `ho'ut` '8' `has` is `ho'ut` '9'

ascii_to_char :: ASCII -> Char
ascii_to_char = is
 `har` lower_latin_to_char
  `has` upper_latin_to_char
 `has_` digit_to_char `ha` is
 `has_` bracket_to_char `ha` is
  `has` punctuation_to_char `ha` is
 `has_` caret_to_char `ha` is

glyph_to_ascii = \case
 ' ' -> Symbol `ha` Punctuate `ha` Space `har` Unit
 '/' -> Symbol `ha` Punctuate `ha` Back `ha` Slash `har` Unit
 '\\' -> Symbol `ha` Punctuate `ha` Slash `har` Unit
 '(' -> Symbol `ha` Bracket `ha` Opened `ha` Round `har` Unit
 ')' -> Symbol `ha` Bracket `ha` Closed `ha` Round `har` Unit
 '{' -> Symbol `ha` Bracket `ha` Opened `ha` Curly `har` Unit
 '}' -> Symbol `ha` Bracket `ha` Closed `ha` Curly `har` Unit
 '<' -> Symbol `ha` Bracket `ha` Opened `ha` Angle `har` Unit
 '>' -> Symbol `ha` Bracket `ha` Closed `ha` Angle `har` Unit
 '[' -> Symbol `ha` Bracket `ha` Opened `ha` Square `har` Unit
 ']' -> Symbol `ha` Bracket `ha` Closed `ha` Square `har` Unit
 '"' -> Symbol `ha` Punctuate `ha` Doublequote `har` Unit
 '\'' -> Symbol `ha` Punctuate `ha` Singlequote `har` Unit
 '.' -> Symbol `ha` Punctuate `ha` Period `har` Unit
 ',' -> Symbol `ha` Punctuate `ha` Comma `har` Unit
 ';' -> Symbol `ha` Punctuate `ha` Semicolon `har` Unit
 ':' -> Symbol `ha` Punctuate `ha` Colon `har` Unit
 '!' -> Symbol `ha` Punctuate `ha` Exclamation `har` Unit
 '?' -> Symbol `ha` Punctuate `ha` Question `har` Unit
 '#' -> Symbol `ha` Punctuate `ha` Hash `har` Unit
 '$' -> Symbol `ha` Punctuate `ha` Dollar `har` Unit
 '%' -> Symbol `ha` Punctuate `ha` Percent `har` Unit
 '&' -> Symbol `ha` Punctuate `ha` Ampersand `har` Unit
 '*' -> Symbol `ha` Punctuate `ha` Asterisk `har` Unit
 '+' -> Symbol `ha` Punctuate `ha` Plus `har` Unit
 '-' -> Symbol `ha` Punctuate `ha` Hyphen `har` Unit
 '=' -> Symbol `ha` Punctuate `ha` Equal `har` Unit
 '@' -> Symbol `ha` Punctuate `ha` At `har` Unit
 '^' -> Symbol `ha` Punctuate `ha` Circumflex `har` Unit
 '_' -> Symbol `ha` Punctuate `ha` Underscore `har` Unit
 '`' -> Symbol `ha` Punctuate `ha` Grave `har` Unit
 '|' -> Symbol `ha` Punctuate `ha` Bar `har` Unit
 '~' -> Symbol `ha` Punctuate `ha` Tilde `har` Unit
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
 '\BS' -> Exist `ha` Caret `ha` Back `ha` Space `har` Unit
 '\HT' -> Exist `ha` Caret `ha` Tab `har` Unit
 '\LF' -> Exist `ha` Caret `ha` Newline `har` Unit
 '\ESC' -> Exist `ha` Caret `ha` Escape `har` Unit
 ' ' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Space `har` Unit
 '\DEL' -> Exist `ha` Caret `ha` Delete `har` Unit
 '/' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Back `ha` Slash `har` Unit
 '\\' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Slash `har` Unit
 '(' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `har'st` Round
 ')' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `har'st` Round
 '{' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `har'st` Curly
 '}' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `har'st` Curly
 '<' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `har'st` Angle
 '>' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `har'st` Angle
 '[' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Opened `har'st` Square
 ']' -> Exist `ha` Glyph `ha` Symbol `ha` Bracket `ha` Closed `har'st` Square
 '"' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Doublequote `har` Unit
 '\'' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Singlequote `har` Unit
 '.' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Period `har` Unit
 ',' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Comma `har` Unit
 ';' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Semicolon `har` Unit
 ':' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Colon `har` Unit
 '!' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Exclamation `har` Unit
 '?' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Question `har` Unit
 '#' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Hash `har` Unit
 '$' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Dollar `har` Unit
 '%' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Percent `har` Unit
 '&' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Ampersand `har` Unit
 '*' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Asterisk `har` Unit
 '+' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Plus `har` Unit
 '-' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Hyphen `har` Unit
 '=' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Equal `har` Unit
 '@' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` At `har` Unit
 '^' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Circumflex `har` Unit
 '_' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Underscore `har` Unit
 '`' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Grave `har` Unit
 '|' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Bar `har` Unit
 '~' -> Exist `ha` Glyph `ha` Symbol `ha` Punctuate `ha` Tilde `har` Unit
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
