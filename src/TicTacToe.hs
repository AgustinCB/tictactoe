module TicTacToe (startGame) where

import TicTacToeGame
import IOHelpers (getUserInput)

import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(NoBuffering))
import Text.ParserCombinators.Parsec (parse, Parser)
import Text.Parsec.Char

checkExit :: Game -> IO ()
checkExit game = case checkFinished game of
  True -> putStrLn (showGameStatus game) >> exitSuccess
  False -> return ()

lowercase :: String -> String
lowercase = map toLower

printRepl :: Game -> IO String
printRepl game = do
  putStrLn (showGame game)
  hFlush stdout
  getUserInput

parseCommand :: String -> Command
parseCommand inp = case (parse commandParser "" (lowercase inp)) of
  Left _ -> Nothing
  Right position -> Just position

humanPlayer :: IA
humanPlayer = parseCommand

getMovements :: Game -> IA
getMovements _ = humanPlayer

run :: Game -> IO ()
run game = do
      checkExit game
      inp <- printRepl game
      run $ updateStatus $ updateGame ((getMovements game) inp) game

startGame :: IO()
startGame = run initialGame
