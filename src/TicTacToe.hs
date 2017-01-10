module TicTacToe (startGame) where

import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.List
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(NoBuffering))
import Text.ParserCombinators.Parsec (parse, Parser)
import Text.Parsec.Char

data Game = Game [[Piece]] Status
data Status = FirstPlayerPlaying | SecondPlayerPlaying | FirstPlayerWon | SecondPlayerWon
data Piece = FirstPlayer | SecondPlayer | Empty
data Position = Position Int Int
type Command = Maybe Position 

showGame :: Game -> String
showGame (Game pieces status) =
  (showStatus status) ++
  (intercalate "\n"
    (map ((intercalate " ") . (map showPiece)) pieces))

showStatus :: Status -> String
showStatus FirstPlayerPlaying = "First Player Playing\n"
showStatus SecondPlayerPlaying = "Second Player Playing\n"

showPiece :: Piece -> String
showPiece FirstPlayer = "X"
showPiece SecondPlayer = "O"
showPiece Empty = "."

initialGame :: Game
initialGame = Game pieces FirstPlayerPlaying
  where pieces = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]

replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = (f x):xs
replace f i (x:xs) = x : replace f (i-1) xs
replace f i [] = []

replace2D :: (a -> a) -> Position -> [[a]] -> [[a]]
replace2D f (Position x y) = replace (replace f y) x

setPosition :: Position -> [[Piece]] -> Piece -> [[Piece]]
setPosition pos pieces piece = replace2D (const piece) pos pieces

updateGame :: Command -> Game -> Game
updateGame Nothing game = game
updateGame (Just position@(Position x y)) (Game pieces FirstPlayerPlaying) = (Game (setPosition position pieces FirstPlayer) SecondPlayerPlaying)
updateGame (Just position@(Position x y)) (Game pieces SecondPlayerPlaying) = (Game (setPosition position pieces SecondPlayer) FirstPlayerPlaying)

updateStatus :: Game -> Game
updateStatus (Game pieces@([[FirstPlayer, FirstPlayer, FirstPlayer], [_, _, _], [_, _, _]]) status) =
  (Game pieces FirstPlayerWon)
updateStatus (Game pieces@([[SecondPlayer, SecondPlayer, SecondPlayer], [_, _, _], [_, _, _]]) status) =
  (Game pieces SecondPlayerWon)
updateStatus (Game pieces@([[_, _, _], [FirstPlayer, FirstPlayer, FirstPlayer], [_, _, _]]) status) =
  (Game pieces FirstPlayerWon)
updateStatus (Game pieces@([[_, _, _], [SecondPlayer, SecondPlayer, SecondPlayer], [_, _, _]]) status) =
  (Game pieces SecondPlayerWon)
updateStatus (Game pieces@([[_, _, _], [_, _, _], [FirstPlayer, FirstPlayer, FirstPlayer]]) status) =
  (Game pieces FirstPlayerWon)
updateStatus (Game pieces@([[_, _, _], [_, _, _], [SecondPlayer, SecondPlayer, SecondPlayer]]) status) =
  (Game pieces SecondPlayerWon)
updateStatus (Game pieces@([[FirstPlayer, _, _], [FirstPlayer, _, _], [FirstPlayer, _, _]]) status) =
  (Game pieces FirstPlayerWon)
updateStatus (Game pieces@([[SecondPlayer, _, _], [SecondPlayer, _, _], [SecondPlayer, _, _]]) status) =
  (Game pieces SecondPlayerWon)
updateStatus (Game pieces@([[_, FirstPlayer, _], [_, FirstPlayer, _], [_, FirstPlayer, _]]) status) =
  (Game pieces FirstPlayerWon)
updateStatus (Game pieces@([[_, SecondPlayer, _], [_, SecondPlayer, _], [_, SecondPlayer, _]]) status) =
  (Game pieces SecondPlayerWon)
updateStatus (Game pieces@([[_, _, FirstPlayer], [_, _, FirstPlayer], [_, _, FirstPlayer]]) status) =
  (Game pieces FirstPlayerWon)
updateStatus (Game pieces@([[_, _, SecondPlayer], [_, _, SecondPlayer], [_, _, SecondPlayer]]) status) =
  (Game pieces SecondPlayerWon)
updateStatus (Game pieces@([[FirstPlayer, _, _], [_, FirstPlayer, _], [_, _, FirstPlayer]]) status) =
  (Game pieces FirstPlayerWon)
updateStatus (Game pieces@([[SecondPlayer, _, _], [_, SecondPlayer, _], [_, _, SecondPlayer]]) status) =
  (Game pieces SecondPlayerWon)
updateStatus (Game pieces@([[_, _, FirstPlayer], [_, FirstPlayer, _], [FirstPlayer, _, _]]) status) =
  (Game pieces FirstPlayerWon)
updateStatus (Game pieces@([[_, _, SecondPlayer], [_, SecondPlayer, _], [SecondPlayer, _, _]]) status) =
  (Game pieces SecondPlayerWon)
updateStatus game = game

checkExit :: Game -> IO ()
checkExit (Game _ FirstPlayerWon) = putStrLn "First player won" >> exitSuccess
checkExit (Game _ SecondPlayerWon) = putStrLn "Second player won" >> exitSuccess
checkExit (Game _ _) = return ()

lowercase :: String -> String
lowercase = map toLower

getUserInput :: IO String
getUserInput = do
            hSetBuffering stdin NoBuffering
            getLine

printRepl :: Game -> IO String
printRepl game = do
  putStrLn (showGame game)
  hFlush stdout
  getUserInput

toInt :: Char -> Int
toInt inp = read([inp]) :: Int

commandParser :: Parser Position
commandParser = do
  x <- digit
  char 'x'
  y <- digit
  return $ Position (toInt x) (toInt y)

parseCommand :: String -> Command
parseCommand inp = case (parse commandParser "" (lowercase inp)) of
  Left _ -> Nothing
  Right position -> Just position

run :: Game -> IO ()
run game = do
      checkExit game
      inp <- printRepl game
      run $ updateStatus $ updateGame (parseCommand inp) game

startGame :: IO()
startGame = run initialGame
