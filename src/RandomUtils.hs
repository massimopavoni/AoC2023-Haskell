module RandomUtils (Parser, Direction (..), parseInput, oppositeDir, movePos, manhattanDistance, position, space) where

import Control.Arrow ((&&&))
import Data.Tuple.Extra (both)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, getSourcePos, parse, some, sourceColumn, sourceLine, unPos)
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

movePos :: Int -> (Int, Int) -> Direction -> (Int, Int)
movePos a (x, y) d = case d of
  S -> (x + a, y)
  E -> (x, y + a)
  N -> (x - a, y)
  W -> (x, y - a)

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

------------------------------------------------------------------------------------------------
-- Parsers

space :: Parser String
space = some $ char ' '

-- The position function could be a bit of a controversial choice,
-- since it's supposedly "not cheap", in terms of computation time.
-- It's true that I usually don't call it on every single character, and that it's necessary
-- for the way I represented some puzzle inputs, but I'm sure there's a better solution.
position :: Parser (Int, Int)
position = both unPos . (sourceLine &&& sourceColumn) <$> getSourcePos
