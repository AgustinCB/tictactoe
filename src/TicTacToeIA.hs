module TicTacToeIA(IA, humanPlayer) where

import TicTacToeGame
import Text.ParserCombinators.Parsec (parse)

import Data.Char (toLower)

type IA = String -> Command

lowercase :: String -> String
lowercase = map toLower

parseCommand :: String -> Command
parseCommand inp = case (parse commandParser "" (lowercase inp)) of
  Left _ -> Nothing
  Right position -> Just position

humanPlayer :: IA
humanPlayer = parseCommand
