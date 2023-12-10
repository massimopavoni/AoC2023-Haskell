module CubeConundrum (CubeColor (..), possibleGame, fewestCubes) where

import CommonUtils (Parser, parseInput)
import Control.Category ((>>>))
import Control.Monad (guard, (>=>))
import Data.Map.Strict (assocs, fromListWith)
import Data.Maybe (fromJust)
import GHC.Utils.Misc (capitalise)
import Text.Megaparsec (eof, sepBy1, some, (<|>))
import Text.Megaparsec.Char (char, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

------------------------------------------------------------------------------------------------
-- Data types

data CubeColor = Blue | Green | Red
  deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is very easy, once the parser for the input is properly set up.
possibleGame :: [(CubeColor, Int)] -> String -> Maybe Int
possibleGame bag =
  parseInput gameParser Just
    >=> liftA2
      (>>)
      (guard . all possibleExtraction . snd)
      (pure . fst)
  where
    possibleExtraction :: (CubeColor, Int) -> Bool
    possibleExtraction (c, n) = fromJust (lookup c bag) >= n

-- The second part of the problem is even simpler.
fewestCubes :: String -> [(CubeColor, Int)]
fewestCubes =
  parseInput
    gameParser
    ( fromListWith max . snd
        >>> assocs
    )

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
    (sepBy1 extraction (string ", " <|> string "; " <|> (eof >> pure "")))
  where
    extraction :: Parser (CubeColor, Int)
    extraction =
      liftA2
        (flip (,))
        (decimal <* char ' ')
        (read . capitalise <$> some letterChar)
