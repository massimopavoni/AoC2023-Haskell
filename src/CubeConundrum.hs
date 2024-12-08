module CubeConundrum (possibleGamesIdSum, fewestCubesPowerSetSum) where

import Control.Category ((>>>))
import Control.Monad (guard, (>=>))
import Data.Map.Strict (assocs, fromListWith)
import Data.Maybe (fromJust, mapMaybe)
import GHC.Utils.Misc (capitalise)
import RandomUtils (Parser, parseInput)
import Text.Megaparsec (choice, eof, sepBy1, some)
import Text.Megaparsec.Char (char, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

------------------------------------------------------------------------------------------------
-- Data types

data CubeColor = Blue | Green | Red
  deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is very easy, once the parser for the input is properly set up.
possibleGamesIdSum :: String -> Int
possibleGamesIdSum =
  lines
    >>> mapMaybe
      ( parseInput gameParser Just
          >=> liftA2
            (>>)
            (guard . all possibleExtraction . snd)
            (pure . fst)
      )
    >>> sum
  where
    possibleExtraction :: (CubeColor, Int) -> Bool
    possibleExtraction (c, n) = fromJust (lookup c bag) >= n
      where
        bag :: [(CubeColor, Int)]
        bag = [(Blue, 14), (Green, 13), (Red, 12)]

-- The second part of the problem is even simpler.
fewestCubesPowerSetSum :: String -> Int
fewestCubesPowerSetSum =
  lines
    >>> map
      ( parseInput
          gameParser
          ( fromListWith max . snd
              >>> assocs
          )
          >>> product . map snd
      )
    >>> sum

------------------------------------------------------------------------------------------------
-- Parsers

-- I ended up using megaparsec again, because it seemed even more appropriate
-- compared to the first challenge; this time the parser is also very simple,
-- probably partly because I'm learning more about the library as I go.
gameParser :: Parser (Int, [(CubeColor, Int)])
gameParser =
  liftA2
    (,)
    (string "Game " *> decimal <* string ": ")
    (sepBy1 extraction (choice [string ", ", string "; ", eof >> pure ""]))
  where
    extraction :: Parser (CubeColor, Int)
    extraction =
      liftA2
        (flip (,))
        (decimal <* char ' ')
        (read . capitalise <$> some letterChar)
