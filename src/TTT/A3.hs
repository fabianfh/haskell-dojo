module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01

showInts:: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

_HEADER_::String
_HEADER_ = " " ++ (formatLine $ showInts _RANGE_ )

-- Q#02

showSquares:: [Square] -> [String]
showSquares [] = []
showSquares (x:xs) = showSquare x : showSquares xs

-- Q#03

formatRows:: Board -> [String]
formatRows [] = []
formatRows (r:rs) =  (formatLine $ showSquares r) : formatRows rs

-- Q#04

isColEmpty:: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty row index = if elem index [0,1,2] then  row !! index == Empty else False 

-- Q#05

dropFirstCol:: Board -> Board
dropFirstCol (r:rs) = tail r : dropFirstCol rs 
dropFirstCol [] = []


dropLastCol:: Board -> Board
dropLastCol (r:rs) = (reverse . tail . reverse) r : dropLastCol rs 
dropLastCol [] = []

-- Q#06

getDiag1:: Board -> Line
getDiag1 (r:rs) = (head r) : getDiag1 (dropFirstCol rs)
getDiag1 [] = []

getDiag2:: Board -> Line
getDiag2 (r:rs) = last r : getDiag2 (dropFirstCol rs)
getDiag2 [] = []


getAllLines:: Board -> [Line]
getAllLines board = board ++ transpose board ++ [getDiag1 board] ++ [getDiag2 board]



-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare player (r:rs) (x,y) = 
    if x == 0 
    then (replaceSquareInRow r player y) : rs
    else r                               : putSquare player rs (x-1,y)
-- Q#08

prependRowIndices::[String] -> [String]
prependRowIndices xs = [(a: ". ") ++ show b | (a,b) <- zip ['A' ..] xs]

-- Q#09

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ player line = all (player ==) line 

-- Q#10
isValidMove:: Board -> Move -> Bool
isValidMove rs (x,y) = case isMoveInBounds (x,y) of
                            True  -> go rs y
                            False -> False
                            where go :: Board -> Int -> Bool
                                  go (r:rs) y = isColEmpty r y || go rs y
                                  go [] _ = False 

 