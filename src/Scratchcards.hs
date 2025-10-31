module Scratchcards (scratchcardPointsSum, scratchcardCloneCountsSum) where

import Control.Category ((>>>))
import RandomUtils (Parser, parseInput)
import Text.Megaparsec (eof, notFollowedBy, sepBy1, try)
import Text.Megaparsec.Char (char, hspace1, string)
import Text.Megaparsec.Char.Lexer (decimal)

-- I guess I'm also taking this first Advent of Code instance of mine as a megaparsec exploration, lol.

---------------------------------------------------------------------------------------------------
-- Exports

-- The first part is again quite straightforward, provided a good simple parser is written.
scratchcardPointsSum :: String -> Int
scratchcardPointsSum =
  lines
    >>> map
      ( parseInput scratchcardParser $
          \(ws, ns) -> 2 ^ length (filter (`elem` ws) ns) `div` 2
      )
    >>> sum

-- The second part requires slightly more thought, as the scratchcards are never gonna cause
-- duplicates outside the already existing ones.
-- Therefore, we can just start from the end of the list, since we know for sure that
-- all the previous scratchcards are gonna have clones only of the ones we already considered,
-- and sum the first values of the subsequent scratchcards, depending on the current one's win count.
scratchcardCloneCountsSum :: String -> Int
scratchcardCloneCountsSum =
  lines
    >>> map (parseInput scratchcardParser id)
    >>> foldr (\sc l -> 1 + sum (take (wins sc) l) : l) []
    >>> sum
  where
    wins :: ([Int], [Int]) -> Int
    wins (ws, ns) = length $ filter (`elem` ws) ns

---------------------------------------------------------------------------------------------------
-- Parsers

scratchcardParser :: Parser ([Int], [Int])
scratchcardParser = do
  _ <- string "Card" *> hspace1 *> decimal <* char ':' <* hspace1 :: Parser Int
  ws <- sepBy1 decimal (try $ hspace1 <* notFollowedBy (char '|')) <* string " |" <* hspace1
  ns <- sepBy1 decimal hspace1 <* eof
  pure (ws, ns)
