module RandomUtils (Parser, Direction (..), parseInput, oppositeDir, movePos, manhattanDistance, space) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, some)
import Text.Megaparsec.Char (char)

------------------------------------------------------------------------------------------------
-- Data types

type Parser = Parsec Void String

data Direction = N | S | W | E
  deriving (Bounded, Enum, Eq)

------------------------------------------------------------------------------------------------
-- Functions

parseInput :: Parser a -> (a -> b) -> String -> b
parseInput p f = either (error . errorBundlePretty) f . parse p ""

oppositeDir :: Direction -> Direction
oppositeDir N = S
oppositeDir S = N
oppositeDir W = E
oppositeDir E = W

movePos :: (Int, Int) -> Direction -> (Int, Int)
movePos (x, y) d = case d of
  N -> (x - 1, y)
  S -> (x + 1, y)
  W -> (x, y - 1)
  E -> (x, y + 1)

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

------------------------------------------------------------------------------------------------
-- Parsers

space :: Parser String
space = some $ char ' '
