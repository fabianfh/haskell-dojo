module TTT.A4 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)

-- Q#01

_HEADER_:: String
_HEADER_ = "_" ++ (concat $ map (\s -> "|_" ++ show(s) ++ "_") _RANGE_ ) ++ "|_"
-- Q#02

showSquares:: [Player] -> [String]
showSquares = map showSquare

-- Q#03
dropFirstCol:: Board -> Board
dropFirstCol board = map tail board

-- Q#04
dropLastCol:: Board -> Board
dropLastCol board = map (reverse.tail.reverse) board

--Q#05
formatRows:: Board -> [String]
formatRows  = map (\row -> (formatLine.showSquares) row )    

-- Q#06

isWinningLine_:: Square -> [Square] -> Bool
isWinningLine_ player ss =  (not.null $ ss)  && (null $ filter (==player) ss )
-- Q#07

isWinningLine:: Square -> [Square] -> Bool
isWinningLine player = foldr (\a -> (\b -> b && (a==player))) True
-- Q#08

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]


-- _EMPTY_BOARD_ = [ [Empty, Empty, Empty]
--                 , [Empty, Empty, Empty]
--                 , [Empty, Empty, Empty]
--                 ]

hasWon:: Player -> Board -> Bool
hasWon player board = elem [player,player,player] (getAllLines board)

-- Q#09
getGameState:: Board -> GameState
getGameState board = case hasWon X board of
                    True -> XWin
                    False-> case hasWon O board of
                            True  -> XLoose
                            False -> case isInProg board of
                                 True  -> InProg 
                                 False -> Draw
                    where isInProg:: Board -> Bool
                          isInProg board = foldr (\a -> (\b -> b && (elem Empty a))) True board


prependRowIndices::[String] -> [String]
-- prependRowIndices xs = [(a: ". ") ++ show b | (a,b) <- zip ['A' ..] xs]
prependRowIndices xs = zipWith (\a -> \b -> (a: ". ") ++  b )['A' ..] xs     


-- Q#11

formatBoard:: Board -> String
formatBoard board = unlines $ (' ' :' ' :' ' : _HEADER_ ): (prependRowIndices. formatRows $ board) 
















