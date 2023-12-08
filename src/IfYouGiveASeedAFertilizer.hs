{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IfYouGiveASeedAFertilizer (nearestSeed, nearestSeedRange) where

import Control.Category ((>>>))
import Control.Monad (void)
import Data.Foldable (find, foldl')
import Data.List (sort)
import Data.List.Extra (chunksOf)
import Data.Maybe (fromJust, isJust)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (notFollowedBy), Parsec, anySingle, between, count, eof, errorBundlePretty, parse, sepBy1, someTill, try, (<|>))
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

-- This one took me a whole lot of time to work out the second part,
-- I found it so difficult, but it was so very worth it,
-- for the immense satisfaction I felt after solving it.

------------------------------------------------------------------------------------------------
-- Data types

data Range a = Range {start :: a, stop :: a}
  deriving (Eq)

-- The Range Functor wasn't really necessary,
-- but it was a the first occasion I had to write an instance of it,
-- and it looked nicer when used in the first case of the mapSeedRange function.
-- (Also, I enabled instance signs because I prefer to have them in function contexts and, well, in instances.)
instance Functor Range where
  fmap :: (a -> b) -> Range a -> Range b
  fmap f (Range s e) = Range (f s) (f e)

-- The Ord instance is necessary to be able to sort ranges (and indeed it only compares based on the start).
instance (Eq a, Ord a) => Ord (Range a) where
  compare :: Range a -> Range a -> Ordering
  compare (Range s1 _) (Range s2 _) = compare s1 s2

data MapRange a = MapRange {range :: Range a, offset :: a}

type Parser = Parsec Void String

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is actually very very simple, as the number of seeds is small,
-- and they can simply be passed through every almanac map without any slowdown.
nearestSeed :: String -> Int
nearestSeed =
  either (error . errorBundlePretty) id . parse (almanacParser id) ""
    >>> (\(ss, ms) -> flip (foldl' passSingleThroughRangeMap) ms <$> ss)
    >>> minimum
  where
    -- Funny thing: this one function gave me the opportuninty to better understand
    -- how functions can become incredily beautiful in their point-free versions,
    -- and I was provided with an opportunity to look at how the liftA2 function works
    -- in the context of Applicatives (also appreciating the evident connection between liftA2 and <*>).
    passSingleThroughRangeMap :: (Num a, Ord a) => a -> [MapRange a] -> a
    passSingleThroughRangeMap i =
      find (liftA2 (&&) ((<= i) . start . range) ((i <=) . stop . range))
        >>> maybe i ((i +) . offset)

-- The second part was a nightmare at the beginning, but it eventually grew on me,
-- and in the end solving it was, as I've already said, very satisfying.
-- The main function just uses the same parser (with a different seed function),
-- but then does not pass the single seed through the maps, but rather whole seed ranges,
-- to then just pick the minimum of their starts, since that's always the smallest value in the range.
nearestSeedRange :: String -> Int
nearestSeedRange =
  either (error . errorBundlePretty) id . parse (almanacParser seedsToRanges) ""
    >>> uncurry (foldl' passRangeThroughRangeMap)
    >>> map start
    >>> minimum
  where
    seedsToRanges :: [Int] -> [Range Int]
    seedsToRanges = map listToRange . chunksOf 2

    -- Using bounds-inclusive ranges makes it difficult to mess up range subtraction,
    -- but I guess I could have done something slightly different in the other functions so that
    -- I didn't have to use bounds-inclusive ranges and so many -1s and +1s.
    listToRange :: (Num a, Ord a) => [a] -> Range a
    listToRange [b, l] = Range b (b + l - 1)
    listToRange _ = error "Invalid seed range length"

    -- This is the main, complex and slightly complicated function for the range through map passing.
    -- The scoped type variables extension was quite useful to still be able to write types for
    -- the local functions without different polymorphic types errors (since they should all be the same,
    -- but the compiler cannot infer that if the signature is explicit).
    passRangeThroughRangeMap :: forall a. (Num a, Ord a) => [Range a] -> [MapRange a] -> [Range a]
    passRangeThroughRangeMap srs mrs = concatMap mapSeedRange srs
      where
        -- Map seed range has three cases:
        -- 1. the seed range is fully contained in a map range, so it can just use that map's offset;
        -- 2. the seed range has some overlap with one or more map ranges, so it has to be split;
        -- 3. the seed range has no kind of match with any map range, so it can just be returned as is
        -- (but inside a list, since the concat above will use a list).
        -- The splitting case is the most complex, and in this function it uses a fold to subtract
        -- the map ranges (only the ones that overlap with the given range) from the seed range;
        -- since those map ranges are ordered, looking at how the subtractRange function
        -- we can just consider the last range as the next one to subtract from.
        mapSeedRange :: Range a -> [Range a]
        mapSeedRange sr
          | isJust inRangeMap = [(+ offset (fromJust inRangeMap)) <$> sr]
          | not $ null filteredOverlappingMapRanges =
              concatMap mapSeedRange
                . foldl' (\rs mr -> subtractRange (last rs) mr ++ init rs) [sr]
                $ sort filteredOverlappingMapRanges
          | otherwise = [sr]
          where
            inRangeMap :: Maybe (MapRange a)
            inRangeMap = find ((`containsRange` sr) . range) mrs

            filteredOverlappingMapRanges :: [Range a]
            filteredOverlappingMapRanges = filter (overlappingRange sr) $ map range mrs

        -- Range A is overlapping with range B if A contains B or if B contains any of A's bounds.
        overlappingRange :: Range a -> Range a -> Bool
        overlappingRange r1@(Range a b) r2 =
          containsRange r1 r2
            || (containsBound r2 a && not (containsBound r2 b))
            || (not (containsBound r2 a) && containsBound r2 b)

        containsRange :: Range a -> Range a -> Bool
        containsRange (Range a b) (Range c d) = a <= c && d <= b

        containsBound :: Range a -> a -> Bool
        containsBound (Range a b) c = a <= c && c <= b

        -- To subtract a range from another (B from A), we have three cases:
        -- 1. B is fully contained in A, A can be split in three ranges (the middle one overlapping);
        -- 2. B contains A's stop, A is split in two ranges (the second one overlapping);
        -- 3. B contains A's start, A is split in two ranges (the first one overlapping).
        -- The last case is one that should never happen, as if A is fully contained in B,
        -- it should've already been mapped by the first case of mapSeedRange.
        subtractRange :: Range a -> Range a -> [Range a]
        subtractRange (Range a b) (Range c d)
          | a <= c && d <= b = [Range a (c - 1), Range c d, Range (d + 1) b]
          | a <= c && b < d = [Range a (c - 1), Range c b]
          | c < a && d <= b = [Range a d, Range (d + 1) b]
          | otherwise = error "Range was supposed to be already mapped"

------------------------------------------------------------------------------------------------
-- Parsers

-- The almanac parser is straightforward: it just parses seeds and maps.
almanacParser :: (Num a) => ([a] -> b) -> Parser (b, [[MapRange Int]])
almanacParser st = do
  ss <- st <$> between (string "seeds: ") (count 2 newline) (sepBy1 decimal (char ' '))
  ms <- sepByEnd mapParser (count 2 newline) eof
  return (ss, ms)
  where
    mapParser :: Parser [MapRange Int]
    mapParser = do
      _ <- someTill anySingle (string "map:" <* newline)
      sepByEnd
        (listToRange <$> sepBy1 decimal (char ' '))
        newline
        (void newline <|> eof)

    listToRange :: (Num a, Ord a) => [a] -> MapRange a
    listToRange [d, s, i] = MapRange (Range s (s + i - 1)) (d - s)
    listToRange _ = error "Invalid map range length"

    -- The only peculiar function is the new parser combinator sepByEnd,
    -- which variates on sepBy1 for cases that could have something different after the last separator.
    -- I'm not 100% sure this was necessary, but I was having some problems with the way the input is formatted,
    -- and thought about this way of using negative lookahead to deal with it.
    sepByEnd :: Parser a -> Parser b -> Parser c -> Parser [a]
    sepByEnd p sep end = sepBy1 p (try $ sep <* notFollowedBy end)