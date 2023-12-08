module Scratchcards (scratchcardPoints, scratchcardsClonesCounts) where

import Control.Category ((>>>))
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, notFollowedBy, parse, sepBy1, some, try)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

-- I guess I'm also taking this first Advent of Code instance of mine as a megaparsec exploration, lol.

------------------------------------------------------------------------------------------------
-- Data types

type Parser = Parsec Void String

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is again quite straightforward, provided a good simple parser is written.
scratchcardPoints :: String -> Int
scratchcardPoints =
  either (error . errorBundlePretty) id . parse scratchcardParser ""
    >>> \(ws, ns) -> 2 ^ length (filter (`elem` ws) ns) `div` 2

-- The second part requires slightly more thought, as the scratchcards are never gonna cause
-- duplicates outside the already existing ones.
-- Therefore, we can just start from the end of the list, since we know for sure that
-- all the previous scratchcards are gonna have clones only of the ones we already considered,
-- and sum the first values of the subsequent scratchcards, depending on the current one's win count.
scratchcardsClonesCounts :: String -> [Int]
scratchcardsClonesCounts =
  lines
    >>> fmap (either (error . errorBundlePretty) id . parse scratchcardParser "")
    >>> foldr (\sc l -> 1 + sum (take (wins sc) l) : l) []
  where
    wins (ws, ns) = length $ filter (`elem` ws) ns

------------------------------------------------------------------------------------------------
-- Parsers

scratchcardParser :: Parser ([Int], [Int])
scratchcardParser = do
  _ <- string "Card" *> space *> decimal <* char ':' <* space :: Parser Int
  ws <- sepBy1 decimal (try $ space <* notFollowedBy (char '|')) <* string " |" <* space
  ns <- sepBy1 decimal space <* eof
  return (ws, ns)
  where
    space :: Parser String
    space = some $ char ' '