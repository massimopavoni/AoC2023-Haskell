{-# LANGUAGE ScopedTypeVariables #-}

module HauntedWasteland (camelEscapeTime, ghostEscapeTime) where

import CommonUtils (Parser, parseInput)
import Control.Applicative (liftA3)
import Control.Category ((>>>))
import Data.List (foldl1')
import Data.Map.Strict (Map, fromList, keys, (!))
import Text.Megaparsec (count, eof, notFollowedBy, sepBy1, some, try, (<|>))
import Text.Megaparsec.Char (char, letterChar, newline, string)

------------------------------------------------------------------------------------------------
-- Data types

-- The Maps data type is a wrapper for a map of nodes and a list of instructions.
-- The map of nodes is always mapping a single node to some collection of other nodes,
-- while the list of instructions is a list of functions that choose from those nodes.
data Maps a b = Maps (Map a b) [b -> a]

------------------------------------------------------------------------------------------------
-- Exports

-- The first part uses the followInstructions function with the ZZZ node end condition, starting from AAA.
camelEscapeTime :: String -> Int
camelEscapeTime = parseInput mapsParser $ flip (followInstructions (== "ZZZ")) "AAA"

-- The second part uses the followInstructions function with any Z ending node end condition,
-- starting from all A ending nodes.
-- The difficult thing is that we shouldn't try and reach the set of Z ending nodes from the set of A ending nodes:
-- that turns out to take a very long time because of not every Z ending node aligning right away,
-- meaning there are disjoint chain loops that will only reach all Z ending nodes together after
-- a certain amount of individual loops.
-- To solve this, we can just follow the instructions for all A ending nodes separately,
-- count the length of those paths, and then find the least common multiple of those lengths.
ghostEscapeTime :: String -> Int
ghostEscapeTime =
  parseInput mapsParser $
    liftA2
      map
      (followInstructions ((== 'Z') . last))
      (filter ((== 'A') . last) . keys . (\(Maps ns _) -> ns))
      >>> foldl1' lcm

------------------------------------------------------------------------------------------------
-- Functions

-- The followInstructions function folds over the maps' instructions starting from begin until end true.
-- The important parts here are:
-- 1. the nextStep function (fst of the next tuple), which applies the instruction to
-- the tuple of the current node one;
-- 2. the end condition itself, as a normal fold would want to fold over the entire list,
-- which in this case will always be infinite because of the cycle function
-- (and the result cannot be short-circuited with a lazy fold either,
-- as we need to count the amount of steps taken).
followInstructions :: forall a b c. (Num a, Ord b) => (b -> Bool) -> Maps b c -> b -> a
followInstructions end (Maps ns is) begin =
  snd . fst $
    until
      (end . fst . fst)
      nextStep
      ((begin, 0), cycle is)
  where
    nextStep :: ((b, a), [c -> b]) -> ((b, a), [c -> b])
    nextStep ((n, c), i : ris) = ((i $ ns ! n, c + 1), ris)
    nextStep _ = error "Instructions folding went wrong"

------------------------------------------------------------------------------------------------
-- Parsers

-- This parser is slightly funnier compared to previous ones, as I wanted to try and
-- parse the nodes list without creating a local scope secondary parser with another do notation.
-- I know it probably still looks bizarre, but trust me, it's much nicer than with two nested liftA2s, lol.
mapsParser :: Parser (Maps String (String, String))
mapsParser = do
  is <- some (fst <$ char 'L' <|> snd <$ char 'R') <* count 2 newline
  ns <-
    sepBy1
      ( liftA3
          (\n l r -> (n, (l, r)))
          (node <* string " = ")
          (char '(' *> node <* string ", ")
          (node <* char ')')
      )
      (try $ newline <* notFollowedBy eof)
      <* newline
      <* eof
  pure $ Maps (fromList ns) is
  where
    node :: Parser String
    node = count 3 letterChar
