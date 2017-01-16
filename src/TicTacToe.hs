module TicTacToe (startGame) where

import TicTacToeGame
import TicTacToeIA
import IOHelpers (getUserInput)

import Control.Monad.IO.Class (liftIO)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(NoBuffering))

checkExit :: Game -> IO ()
checkExit game = case checkFinished game of
  True -> putStrLn (showGameStatus game) >> exitSuccess
  False -> return ()

printRepl :: Game -> IO String
printRepl game = do
  putStrLn (showGame game)
  hFlush stdout
  getUserInput

getMovements :: Game -> IA
getMovements _ = humanPlayer

run :: Game -> IO ()
run game = do
      checkExit game
      inp <- printRepl game
      run $ updateStatus $ updateGame ((getMovements game) inp) game

startGame :: IO()
startGame = run initialGame
