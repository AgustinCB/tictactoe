module TicTacToeIA(IA, humanPlayer) where

import TicTacToeGame
import IOHelpers (getUserInput)
import ArrayHelpers (pick)

import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Text.ParserCombinators.Parsec (parse)

lowercase :: String -> String
lowercase = map toLower

parseCommand :: String -> Command
parseCommand inp = case (parse commandParser "" (lowercase inp)) of
  Left _ -> Nothing
  Right position -> Just position

humanPlayer :: Game -> Command
humanPlayer _ = parseCommand (liftIO getUserInput)

randomPlayer :: IA
randomPlayer game = Just (do (pick (getEmptyPositions game)))
