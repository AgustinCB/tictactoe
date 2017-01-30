module Main where

import TicTacToe

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  startGame args
