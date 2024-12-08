module CosmicExpansion (shortestGalaxyPathsSum, hugeExpansionGalaxyPathsSum) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.List (tails)
import Data.Matrix (Matrix, fromLists, getCol, getRow, matrix, ncols, nrows, unsafeGet)
import Data.Tuple.Extra (both)
import Data.Vector (Vector, fromList, scanl', unsafeIndex)
import qualified Data.Vector as Vect (all)
import RandomUtils (Pos, manhattanDistance)
import Safe (headErr, tailSafe)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part finds the shortest paths using an expanding factor of 2 for empty rows/columns.
shortestGalaxyPathsSum :: String -> Int
shortestGalaxyPathsSum =
  fromLists . lines
    >>> analyzeImage 2
    >>> sum

-- The second part finds the shortest paths using an expanding factor of 1000000 for empty rows/columns.
hugeExpansionGalaxyPathsSum :: String -> Int
hugeExpansionGalaxyPathsSum =
  fromLists . lines
    >>> analyzeImage 1000000
    >>> sum

------------------------------------------------------------------------------------------------
-- Functions

-- During the first part I had a much quicker solution, which just used the Manhattan distance after
-- expanding the Matrix itself, with submatrices splitting and horizontal joining,
-- using the same function for columns too, transposing before and after.
-- I would say to myself that it was pretty clever, but of course the second part had to ruin that all.
-- Since we cannot expand the Matrix adding a million rows/columns everytime we find an all empty one,
-- we just find the indices of the empty rows/columns and then check how many of those
-- the galaxy pairs Manhattan paths would pass through, so the distance formula has that additional term.
-- To be fair, this is the first problem where the solution ends up being pretty slow:
-- I tried using many different data structures and functions, but in the end this is the best I can do
-- without checking out other solutions which I know are very much cleaner and faster.
-- As a small reflection, I do know I'm probably over-engineering most of the solutions,
-- but I'm also having fun learning and doing almost entirely everything on my own.
analyzeImage :: Int -> Matrix Char -> [Int]
analyzeImage e m =
  [(x, y) | x <- [1 .. nrows m], y <- [1 .. ncols m], unsafeGet x y m == '#']
    & ( tails
          >>> concatMap
            ( liftA2 map ((,) . headErr) tailSafe
                >>> map expandedManhattanDistance
            )
      )
  where
    expandedManhattanDistance :: (Pos, Pos) -> Int
    expandedManhattanDistance ((a, b), (c, d)) =
      manhattanDistance (a, b) (c, d)
        + (e - 1)
          * ( unsafeGet (min a c) (max a c) spaceRowsDiffs
                + unsafeGet (min b d) (max b d) spaceColsDiffs
            )

    spaceRowsDiffs :: Matrix Int
    spaceRowsDiffs = spaceDiffs nrows getRow

    spaceColsDiffs :: Matrix Int
    spaceColsDiffs = spaceDiffs ncols getCol

    spaceDiffs :: (Matrix Char -> Int) -> (Int -> Matrix Char -> Vector Char) -> Matrix Int
    spaceDiffs sf gf = matrix (sf m) (sf m) (uncurry (flip (-)) . both (spacesCounts `unsafeIndex`))
      where
        spacesCounts :: Vector Int
        spacesCounts =
          scanl'
            (\acc x -> acc + if x `elem` spaces then 1 else 0)
            0
            (fromList [1 .. sf m])

        spaces :: [Int]
        spaces = filter (Vect.all (== '.') . (`gf` m)) [1 .. sf m]