module TicTacToeIA(IA, humanPlayer, randomPlayer) where

import TicTacToeGame
import IOHelpers (getUserInput)
import ArrayHelpers (pick)

import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Text.ParserCombinators.Parsec (parse)

lowercase :: String -> String
lowercase = map toLower

parseCommand :: IO String -> IO Command
parseCommand inp = do
  userInp <- inp
  return (case (parse commandParser "" (lowercase userInp)) of
    Left _ -> Nothing
    Right position -> Just position)

humanPlayer :: IA
humanPlayer _ = parseCommand getUserInput

randomPlayer :: IA
randomPlayer game = (pick (getPossibleCommands game))
