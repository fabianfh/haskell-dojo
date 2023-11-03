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
printLogo = fmap putStr (readFile _LOGO_PATH_)   

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer = undefined

-- Q#04

getMove = undefined

-- Q#05

play = undefined

-- Q#06

runTTT :: IO ()
runTTT = putStrLn "Not implemented... yet!"

-- Q#07

printLogoDo = undefined

-- Q#08

firstPlayerDo = undefined

-- Q#09

getMoveDo = undefined

-- Q#10

playDo = undefined