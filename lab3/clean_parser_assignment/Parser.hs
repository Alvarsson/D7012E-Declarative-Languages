module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

--iterate parser as long as it succeds
iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

--ADDED, should return result of n from (m # n). This would return result of n since the snd is applied to m # n expression. Usin snd from Prelude.
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd
--m -# n = error "-# not implemented"

--Same as -# but the first element fst from Prelude.
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst
--m #- n = error "#- not implemented"

--ADDED  use iter as long as its parses succeds
spaces :: Parser String
spaces = iter (char ? isSpace)
--spaces =  error "spaces not implemented"

--removes whitespace after accepted string
token :: Parser a -> Parser a
token m = m #- spaces

--ADDED
letter :: Parser Char
letter = char ? isAlpha --prelude function isAlpha
--letter =  error "letter not implemented"

word :: Parser String
word = token (letter # iter letter >-> cons)

--ADDED use method of iterate from "parsing with haskell" cons is to help with head/tail of list, string in this case
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons  
--chars n =  error "chars not implemented"

--twochars :: Parser (Char, Char)
twochars :: Parser String
twochars = chars 2  

--use "m ! n" so as long as parser m doesn't fail n will not be applied to input. Returns error instead of "Nothing"
require :: String -> Parser String 
require w = token (chars (length w)) ? (==w) ! err w

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

--require :: String -> Parser String
--require w  = error "require not implemented"

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

semicolon :: Parser Char 
semicolon = lit ';' -- check with lit that char is exactly ";"

toCap :: Parser Char 
toCap  = letter >-> toUpper

--Assuming this is dead since no redefine was required.
--sndChar :: Parser Char 
--sndChar = twochars >-> snd 

--Changed name for iterate but do not see why as we have iterate
iterater :: Parser a -> Int -> Parser [a]
iterater m 0 = return []
iterater  m i = m # iterater m (i-1) >-> cons

--parser for string of letters. This does accept empty string "".
letters :: Parser String 
letters = iter letter
--letters  = letter # iter letter >-> cons


double :: Parser Char 
double = char #> lit

bldNumber :: Int -> Int -> Int 
bldNumber n d = 10*n+d



-- Not specified what becomes should do
--becomes :: Parser (Char, Char)
--becomes

--isnt this redundant given letter and digit?
--alphanum :: Parser Char 
--alphanum c = 
    




