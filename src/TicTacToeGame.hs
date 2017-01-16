module TicTacToeGame (Game, Command, Position, showGameStatus, showGame,
        checkFinished, initialGame, updateStatus, updateGame, commandParser) where

import Data.List
import ArrayHelpers (replace2D)
import IOHelpers (toInt)
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Char

data Game = Game [[Piece]] Status
data Status = FirstPlayerPlaying | SecondPlayerPlaying | FirstPlayerWon | SecondPlayerWon
data Piece = FirstPlayer | SecondPlayer | Empty
data Position = Position Int Int
type Command = Maybe Position 

-- getEmptyPositions :: Game -> [Position]
-- getEmptyPositions (Game pices _) = (map () pieces)

showGameStatus :: Game -> String
showGameStatus (Game _ status) = showStatus status

showStatus :: Status -> String
showStatus FirstPlayerPlaying = "First Player Playing\n"
showStatus SecondPlayerPlaying = "Second Player Playing\n"
showStatus FirstPlayerWon = "First Player Won\n"
showStatus SecondPlayerWon = "Second Player Won\n"

showPiece :: Piece -> String
showPiece FirstPlayer = "X"
showPiece SecondPlayer = "O"
showPiece Empty = "."

showGame :: Game -> String
showGame (Game pieces status) =
  (showStatus status) ++
  (intercalate "\n"
    (map ((intercalate " ") . (map showPiece)) pieces))

checkFinished :: Game -> Bool
checkFinished (Game _ FirstPlayerPlaying) = False
checkFinished (Game _ SecondPlayerPlaying) = False
checkFinished _ = True

initialGame :: Game
initialGame = Game pieces FirstPlayerPlaying
  where pieces = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]

setPosition :: Position -> [[Piece]] -> Piece -> [[Piece]]
setPosition (Position x y) pieces piece = replace2D (const piece) x y pieces

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

commandParser :: Parser Position
commandParser = do
  x <- digit
  char 'x'
  y <- digit
  return $ Position (toInt x) (toInt y)
