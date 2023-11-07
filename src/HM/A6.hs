module HM.A6 where

import Data.Char (isAlpha)
import HM.Provided






-- Q#01
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

-- Q#02
data GameException = InvalidChars | InvalidLength| NotInDict | InvalidMove | RepeatMove | GameOver



-- Q#03
lengthInRange:: Secret -> Bool
lengthInRange sec = len >= fst _LENGTH_   && len <= snd _LENGTH_
                    where len = foldr (\a b -> b+1 ) 0 sec 

-- Q#04
invalidMove:: Move -> Bool
invalidMove move = elem move (['A' .. 'Z'] ++ ['a' .. 'z'])

-- Q#05
revealLetters:: Move -> Secret -> Guess -> Guess
revealLetters m (c:cs) (g:gs) = if m == c then c:(revealLetters m cs gs) else '_':(revealLetters m cs gs) 
revealLetters _ [] _ = [] 
revealLetters _  _  [] = [] 

-- Q#06
updateChances:: Move -> Secret -> Chances -> Chances
updateChances m s c = if (any (\l -> l == m) s) then c +1 else c 

-- Q#07
setSecret:: IO String
-- setSecret = do
--                 putStr "Enter a secret word:\t"
--                 showInput False
--                 s <- getLine
--                 showInput True
--                 _SPACE_
--                 return s


setSecret = putStr "Enter a secret word:\t" >>
                   showInput False >>
                   getLine >>= \s ->
                   showInput True >>
                   _SPACE_ >>
                   return s