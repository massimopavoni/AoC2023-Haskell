{-# LANGUAGE TupleSections #-}

module GearRatios (partNumbers, gearRatios) where

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Functor ((<&>))
import Data.Map.Strict (elems, fromListWith)
import qualified Data.Map.Strict as Map (filter)
import Data.Matrix (Matrix, fromLists, safeGet)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (fromList, toList)
import Data.Tuple.Extra (both)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, getSourcePos, many, noneOf, optional, parse, some, someTill, sourceColumn, sourceLine, unPos)
import Text.Megaparsec.Char (digitChar, newline)

-- This problem was...oh so much more challenging.
-- I took me three days, with all the time I could spare, to get it right.
-- It might not be the most elegant solution, but it's mine, I like it,
-- it made me learn many more things, and it doesn't look too bad.

------------------------------------------------------------------------------------------------
-- Data types

type Parser = Parsec Void String

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is the most important, as I had to find a way of dealing with the grid-like input.
partNumbers :: String -> [Int]
partNumbers =
  either (error . errorBundlePretty) id
    . ( flip parse ""
          =<< enginePartsParser . fromLists . lines
      )

-- The second part was quite a lot easier, considering the amount of hours I spent on the first one.
gearRatios :: String -> [Int]
gearRatios =
  either (error . errorBundlePretty) id
    . ( flip parse ""
          =<< engineGearsParser . fromLists . lines
      )
    >>> map product

------------------------------------------------------------------------------------------------
-- Parsers

-- I used megaparsec again, just because in my mind there was no way
-- I was gonna be able to correcly parse the parts numbers and
-- the near symbols positions without graually working through the matrix.
enginePartsParser :: Matrix Char -> Parser [Int]
enginePartsParser m = catMaybes <$> someTill maybePart eof
  where
    -- The maybePart parser searches for a sequence of digits between dots and other symbols,
    -- then uses the provided function to decide if the number is a part number.
    -- The one thing that allows this is checking every neighbor of every digit,
    -- and that's only possible thanks to the position function.
    maybePart :: Parser (Maybe Int)
    maybePart = betweenSymbols nearSymbol

    nearSymbol :: Maybe [((Int, Int), Char)] -> Maybe Int
    nearSymbol = maybe Nothing $ \z -> do
      guard $ any (any symbolNeighbor . neighbors m . fst) z
      Just . read $ snd <$> z

    symbolNeighbor :: ((Int, Int), Char) -> Bool
    symbolNeighbor = (`notElem` '\n' : '.' : ['0' .. '9']) . snd

-- The engineGearsParser does a tiny bit more than what the above enginePartsParser does.
-- It's a crucial part of the problem, which I actually solved quite elegantly, in my opinion:
-- the function applied to the tokens parsed by multiple maybeGears is essentially a pipeline that
-- 1. filters out all the Nothing values (the numbers that are not parts or the parts that are not near gears)
-- 2. creates a Map with gears positions to lists of their near parts (effectively grouping the parts to be then multiplied)
-- 3. filters out all gears that could not result in a ratio (the ones that don't have exactly two near parts)
-- 4. gets the values of the Map
engineGearsParser :: Matrix Char -> Parser [[Int]]
engineGearsParser m =
  someTill maybeGears eof
    <&> ( catMaybes
            >>> fromListWith (++) . map (pure <$>) . concat
            >>> Map.filter ((== 2) . length)
            >>> elems
        )
  where
    -- The maybeGears parser is the equivalent of the maybePart parser,
    -- just for the second part of the problem.
    -- Nothing too strange here, other than the final result not containing just one part number,
    -- but a list of gear positions (to later use for grouping) and corresponding part numbers.
    maybeGears :: Parser (Maybe [((Int, Int), Int)])
    maybeGears = betweenSymbols nearGear

    nearGear :: Maybe [((Int, Int), Char)] -> Maybe [((Int, Int), Int)]
    nearGear = maybe Nothing $ \z -> do
      let gears = gearNeighbors z
      guard . not . null $ gears
      Just $ (,read $ snd <$> z) <$> gears

    gearNeighbors :: [((Int, Int), Char)] -> [(Int, Int)]
    gearNeighbors =
      concatMap (neighbors m . fst)
        >>> toList . fromList
        >>> fmap fst . filter ((== '*') . snd)

betweenSymbols :: (Maybe [((Int, Int), Char)] -> Maybe b) -> Parser (Maybe b)
betweenSymbols f =
  between (many noDigit) (many noDigit <* optional newline) $
    f <$> optional (some $ (,) <$> position <*> digitChar)
  where
    noDigit :: Parser Char
    noDigit = noneOf $ '\n' : ['0' .. '9']

    -- The position function allowed me to use a more interesting Arrow combinator
    -- for the first time, and I'm glad I did, 'cause it looks sick.
    -- In all seriousness, the position function could be a bit of a controversial choice,
    -- since it's supposedly "not cheap", in terms of computation time.
    -- It's true that I don't call it on every single character, and that it's necessary
    -- for the way I represented the engine and solve the entire problem,
    -- but I'm sure there's a better solution.
    position :: Parser (Int, Int)
    position = both unPos . (sourceLine &&& sourceColumn) <$> getSourcePos

------------------------------------------------------------------------------------------------
-- Functions

-- Neighbors is maybe the least complex function of all of this,
-- and it got slightly better once I decided to go back to the Matrix type,
-- instead of using an Array, since the first one has consistent indexing bounds
-- in relation to the way megaparsec gives you the current source position.
neighbors :: Matrix Char -> (Int, Int) -> [((Int, Int), Char)]
neighbors m = mapMaybe safeNeighbor . positions
  where
    safeNeighbor :: (Int, Int) -> Maybe ((Int, Int), Char)
    safeNeighbor p = (p,) <$> uncurry safeGet p m

    positions :: (Int, Int) -> [(Int, Int)]
    positions (x, y) = [(l, c) | l <- [x - 1 .. x + 1], c <- [y - 1 .. y + 1], (l, c) /= (x, y)]
