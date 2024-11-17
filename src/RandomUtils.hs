module RandomUtils (Parser, Pos, Direction (..), parseInput, oppositeDir, movePos, manhattanDistance) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

------------------------------------------------------------------------------------------------
-- Data types

type Parser = Parsec Void String

type Pos = (Int, Int)

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

movePos :: Int -> Pos -> Direction -> Pos
movePos a (x, y) d = case d of
  S -> (x + a, y)
  E -> (x, y + a)
  N -> (x - a, y)
  W -> (x, y - a)

manhattanDistance :: Pos -> Pos -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
