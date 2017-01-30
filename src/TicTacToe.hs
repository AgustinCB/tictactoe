module TicTacToe (startGame) where

import TicTacToeGame
import TicTacToeIA

import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(NoBuffering))

checkExit :: Game -> IO ()
checkExit game = case checkFinished game of
  True -> putStrLn (showGameStatus game) >> exitSuccess
  False -> return ()

printRepl :: Game -> IO ()
printRepl game = do
  putStrLn (showGame game)
  hFlush stdout

getMovements :: Game -> [IA] -> Command
getMovements game ias = (getPlayerMovement game ias)

getIA :: String -> IA
getIA "--human" = humanPlayer
getIA "--random" = randomPlayer

getIAs :: [String] -> [IA]
getIAs [] = [humanPlayer humanPlayer]
getIAs (first:second:xs) = [(getIA first) (getIA second)]
getIAs (second:xs) = [humanPlayer (getIA second)]


run :: [IA] -> Game -> IO ()
run ias game = do
      checkExit game
      printRepl game
      run ias $ updateStatus $ updateGame (getMovements game ias) game

startGame :: [String] -> IO()
startGame args = run (getIAs args) initialGame
