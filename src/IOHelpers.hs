module IOHelpers (toInt, getUserInput) where

import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

getUserInput :: IO String
getUserInput = do
  hSetBuffering stdin NoBuffering
  getLine

toInt :: Char -> Int
toInt inp = read([inp]) :: Int
