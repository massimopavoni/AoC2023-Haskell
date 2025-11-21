{-# LANGUAGE TupleSections #-}

module LavaductLagoon (lagoonArea, lagoonAreaFixed) where

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)
import RandomUtils (Direction (..), Pos, manhattanDistance, movePos)
import Safe (headErr, tailSafe)

---------------------------------------------------------------------------------------------------
-- Exports

-- The first part uses the supposed good dig plan instructions.
lagoonArea :: String -> Int
lagoonArea = parseDigPlan >>> calculateArea

-- The second part uses the actual dig plan instructions.
lagoonAreaFixed :: String -> Int
lagoonAreaFixed = parseDigPlanFixed >>> calculateArea

---------------------------------------------------------------------------------------------------
-- Functions

-- This problem is another with a closed form solution, finally. Not much obscure recursion here.
-- The planned area of the lagoon can be calculated using the shoelace formula for the total area,
-- then Pick's theorem for the interior area. We do this because the boundary also has sort of an area,
-- given by the sum of the Manhattan distances between any sequence of coordinates that respect the dig plan.
calculateArea :: [(Direction, Int)] -> Int
calculateArea =
    foldl' (\ps (d, a) -> movePos a (headErr ps) d : ps) [(0, 0)]
        >>> shoelaceArea &&& boundaryLength
        >>> uncurry pickThickBoundary
  where
    shoelaceArea :: [Pos] -> Int
    shoelaceArea ps =
        ps
            & ( (,map snd shifted) . map fst &&& (,map fst shifted) . map snd
                    >>> both (sum . uncurry (zipWith (*)))
                    >>> uncurry (-)
                    >>> (`div` 2)
              )
      where
        shifted :: [Pos]
        shifted = tailSafe $ cycle ps

    boundaryLength :: [Pos] -> Int
    boundaryLength = sum . (zipWith manhattanDistance <*> tailSafe)

    pickThickBoundary :: Int -> Int -> Int
    pickThickBoundary area boundary = area + boundary `div` 2 + 1

---------------------------------------------------------------------------------------------------
-- Parsers

parseDigPlan :: String -> [(Direction, Int)]
parseDigPlan = map (listToDirA . init . words) . lines
  where
    listToDirA :: [String] -> (Direction, Int)
    listToDirA [d, a] = (stringToDir d, read a)
    listToDirA _ = error "Invalid dig plan"

    stringToDir :: String -> Direction
    stringToDir = fromJust . (`lookup` zip (pure <$> "DRUL") [S ..])

parseDigPlanFixed :: String -> [(Direction, Int)]
parseDigPlanFixed = map (hexToDirA . last . words) . lines
  where
    hexToDirA :: String -> (Direction, Int)
    hexToDirA =
        take 6 . drop 2
            >>> charToDir . last &&& read . ("0x" ++) . init

    charToDir :: Char -> Direction
    charToDir = fromJust . (`lookup` zip "1032" [S ..])
