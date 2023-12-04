module CubeConundrum (CubeColor (..), fewestCubes, possibleGame) where

import Control.Arrow (second)
import Control.Monad (guard)
import Data.Map.Strict (assocs, fromListWith)
import Data.Maybe (fromJust)
import Data.Void (Void)
import GHC.Utils.Misc (capitalise)
import Text.Megaparsec (MonadParsec (eof), Parsec, parse, some, (<|>))
import Text.Megaparsec.Char (letterChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

data CubeColor = Blue | Green | Red
  deriving (Eq, Ord, Read, Show)

-- The first part is very easy, once the parser for the input is properly set up.
possibleGame :: [(CubeColor, Int)] -> String -> Maybe Int
possibleGame bag =
  (\(g, b) -> guard b >> Just g)
    . second (all (\(c, n) -> fromJust (lookup c bag) >= n))
    . either (error . show) id
    . parse gameParser ""

type Parser = Parsec Void String

-- I ended up using megaparsec again, because it seemed even more appropriate
-- compared to the first challenge; this time the parser is also very simple,
-- probably partly because I'm learning more about the library as I go.
gameParser :: Parser (Int, [(CubeColor, Int)])
gameParser = do
  game <- string "Game " *> decimal <* string ": "
  extractions <- some extractionsParser
  return (game, extractions)

extractionsParser :: Parser (CubeColor, Int)
extractionsParser = do
  count <- decimal <* space
  color <- read . capitalise <$> some letterChar <* (string ", " <|> string "; " <|> (eof >> return ""))
  return (color, count)

-- The second part of the problem is even simpler.
fewestCubes :: String -> [(CubeColor, Int)]
fewestCubes =
  assocs
    . fromListWith max
    . snd
    . either (error . show) id
    . parse gameParser ""