module CubeConundrum (CubeColor (..), possibleGame, fewestCubes) where

import Control.Category ((>>>))
import Control.Monad (guard, (>=>))
import Data.Map.Strict (assocs, fromListWith)
import Data.Maybe (fromJust)
import Data.Void (Void)
import GHC.Utils.Misc (capitalise)
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, parse, sepBy1, some, (<|>))
import Text.Megaparsec.Char (char, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

------------------------------------------------------------------------------------------------
-- Data types

data CubeColor = Blue | Green | Red
  deriving (Eq, Ord, Read, Show)

type Parser = Parsec Void String

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is very easy, once the parser for the input is properly set up.
possibleGame :: [(CubeColor, Int)] -> String -> Maybe Int
possibleGame bag =
  either (error . errorBundlePretty) Just . parse gameParser ""
    >=> \(g, es) -> guard (all possibleExtraction es) >> return g
  where
    possibleExtraction :: (CubeColor, Int) -> Bool
    possibleExtraction (c, n) = fromJust (lookup c bag) >= n

-- The second part of the problem is even simpler.
fewestCubes :: String -> [(CubeColor, Int)]
fewestCubes =
  either (error . errorBundlePretty) id . parse gameParser ""
    >>> fromListWith max . snd
    >>> assocs

------------------------------------------------------------------------------------------------
-- Parsers

-- I ended up using megaparsec again, because it seemed even more appropriate
-- compared to the first challenge; this time the parser is also very simple,
-- probably partly because I'm learning more about the library as I go.
gameParser :: Parser (Int, [(CubeColor, Int)])
gameParser =
  (,)
    <$> (string "Game " *> decimal <* string ": ")
    <*> sepBy1 extraction (string ", " <|> string "; " <|> (eof >> return ""))
  where
    extraction :: Parser (CubeColor, Int)
    extraction =
      flip (,)
        <$> (decimal <* char ' ')
        <*> (read . capitalise <$> some letterChar)
