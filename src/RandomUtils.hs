module RandomUtils (Parser, Direction (..), parseInput, oppositeDir, movePos, manhattanDistance, space) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, some)
import Text.Megaparsec.Char (char)

------------------------------------------------------------------------------------------------
-- Data types

type Parser = Parsec Void String

data Direction = S | E | N | W
  deriving (Bounded, Enum, Eq, Ord, Show)

------------------------------------------------------------------------------------------------
-- Functions

parseInput :: Parser a -> (a -> b) -> String -> b
parseInput p f = either (error . errorBundlePretty) f . parse p ""

oppositeDir :: Direction -> Direction
oppositeDir S = N
oppositeDir E = W
oppositeDir N = S
oppositeDir W = E

movePos :: (Int, Int) -> Direction -> (Int, Int)
movePos (x, y) d = case d of
  S -> (x + 1, y)
  E -> (x, y + 1)
  N -> (x - 1, y)
  W -> (x, y - 1)

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

------------------------------------------------------------------------------------------------
-- Parsers

space :: Parser String
space = some $ char ' '
