module PointOfIncidence (mirrorScoresSum, mirrorSmudgeScoresSum) where

import Control.Arrow (second, (&&&))
import Control.Category ((>>>))
import Data.List (elemIndex, inits, tails, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both)
import Safe (tailSafe)

---------------------------------------------------------------------------------------------------
-- Exports

-- The first part got changed after the second one, using a generic function.
mirrorScoresSum :: String -> Int
mirrorScoresSum =
  splitOn "\n\n"
    >>> map (reflectionLineScore 0)
    >>> sum

-- The second part introduces the possibility of mirrors with imperfect reflections (smudges).
mirrorSmudgeScoresSum :: String -> Int
mirrorSmudgeScoresSum =
  splitOn "\n\n"
    >>> map (reflectionLineScore 1)
    >>> sum

---------------------------------------------------------------------------------------------------
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
    -- between the sub-lines left and right of the potential reflection line.
    lineReflectionDifferences :: String -> [Int]
    lineReflectionDifferences =
      liftA2
        (zipWith (zipWith (==)))
        (map reverse . tailSafe . inits)
        (init . tailSafe . tails)
        >>> map (length . filter not)
