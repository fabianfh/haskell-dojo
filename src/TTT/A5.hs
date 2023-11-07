module TTT.A5 where

import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)
import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4

-- Q#01

printBoard:: Board -> IO ()
printBoard board = putStrLn.formatBoard $ board

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

printLogo :: IO ()
-- printLogo = readFile _LOGO_PATH_ >>= putStrLn
printLogo = return "********* Here comes the logo **********" >>= putStrLn

-- Q#03
--_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer:: IO Player
-- firstPlayer = _RANDOM_BOOL_ >>= \b -> return (getFirstPlayer b)
firstPlayer = do 
                r <- _RANDOM_BOOL_
                return (getFirstPlayer r)
-- Q#04

getMove:: Board -> IO Move
getMove board =  getLine >>= \s ->  let move = (stringToMove s) 
                                    in
                                        if isValidMove board move 
                                            then return move 
                                            else putStrLn "Invalid move! Try again" >> getMove board 
                                    
                                
    
    
    
-- Q#05
play:: Board -> Player -> IO ()
play board player =  printLogo 
                     >>   printBoard board 
                     >>   promptPlayer player
                     >>   getMove board
                     >>=  \move -> case playMove player board move of
                                   (InProg,nb) -> play nb (switchPlayer player) 
                                   (otherState,nb) -> putStrLn (showGameState otherState)

-- Q#06

runTTT :: IO ()
runTTT = firstPlayer >>= (play _EMPTY_BOARD_)

-- Q#07

printLogoDo = undefined

-- Q#08

firstPlayerDo = undefined

-- Q#09

getMoveDo = undefined

-- Q#10

playDo = undefined


