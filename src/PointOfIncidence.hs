module PointOfIncidence (mirrorScore, mirrorSmudgeScore) where

import Control.Arrow (second, (&&&))
import Control.Category ((>>>))
import Data.List (elemIndex, inits, tails, transpose)
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part got changed after the second one, using a generic function.
mirrorScore :: String -> Int
mirrorScore = reflectionLineScore 0

-- The second part introduces the possibility of mirrors with imperfect reflections (smudges).
mirrorSmudgeScore :: String -> Int
mirrorSmudgeScore = reflectionLineScore 1

------------------------------------------------------------------------------------------------
-- Functions

-- The main dataflow in the reflectionLineScore function is:
-- 1. get the mirror lines;
-- 2. fan-out the lines to start a tuple arrow flow and transpose snd to operate on rows;
-- 3. effectively find the reflection line:
-- -- 3.1 get the reflection differences for every line and transpose them to sum the columns/rows;
-- -- 3.2 search for the one reflection line (or the reflection line with some smudges)
-- -- and sum 1 for the weird indexed columns/rows (-1 is the default for Nothing
-- -- so that it cancels out and we can later sum that to the other tuple element);
-- 4. multiply the row index by 100;
-- 5. sum the tuple elements.
reflectionLineScore :: Int -> String -> Int
reflectionLineScore smudgeCount =
  lines
    >>> (id &&& transpose)
    >>> both
      ( map sum . transpose . map lineReflectionDifferences
          >>> (+ 1) . fromMaybe (-1) . elemIndex smudgeCount
      )
    >>> second (* 100)
    >>> uncurry (+)
  where
    -- The function above makes use of lineReflectionDifferences to find the numbers of different symbols
    -- between the possible sub-lines left and right of the potential reflection line.
    lineReflectionDifferences :: String -> [Int]
    lineReflectionDifferences l = map (length . filter not) $ zipWith (zipWith (==)) left right
      where
        left :: [String]
        left = map reverse . tail $ inits l

        right :: [String]
        right = init . tail $ tails l
