module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01

promptPlayer:: Player -> IO () 
promptPlayer pler =  print  $ concat ["Player ", show pler, " 's turn: enter a row and column position (ex. A1)"]
-- Q#02

_RANGE_ = [1 .. _SIZE_]

-- Q#03

isDigit:: Char -> Bool
isDigit c =elem c ['0' .. '9'] 

readDigit:: Char -> Int
readDigit c = if isDigit c then read [c] else -1

-- Q#04

_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_:: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied:: Board -> Bool
isTied board = all isRowTied board
    where
        isRowTied:: Row -> Bool 
        isRowTied row = all (/= Empty) row 

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]]


-- Q#06


indexRowStrings:: [String] -> [(Char,String)]
indexRowStrings row  = zip ['A' ..] row 

-- Q#07

formatLine:: [String] -> String
formatLine row = concat [_SEP_, (intercalate _SEP_ row), _SEP_]

-- Q#08

isMoveInBounds:: Move -> Bool
isMoveInBounds (x,y)= and [x < _SIZE_, x>= 0,y < _SIZE_, y>= 0] 

-- Q#09

stringToMove:: [Char] -> Move 
stringToMove  [a,b] = if isMoveInBounds mv then mv else _INVALID_MOVE_
                      where fst = (convertRowIndex a) 
                            snd = (readDigit b) -1
                            mv = (fst,snd)
stringToMove _ = _INVALID_MOVE_

-- Q#10

replaceSquareInRow = undefined




















