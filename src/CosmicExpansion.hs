module CosmicExpansion (shortestGalaxyPaths, hugeExpansionGalaxyPaths) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.List (tails)
import Data.Matrix (Matrix, fromLists, getCol, getRow, matrix, ncols, nrows, unsafeGet)
import Data.Tuple.Extra (both)
import Data.Vector (Vector, fromList, scanl', unsafeIndex)
import qualified Data.Vector as Vector (all)
import Safe (tailSafe)

------------------------------------------------------------------------------------------------
-- Data types

data Quadrant = Space | Galaxy
  deriving (Eq)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part finds the shortest paths using an expanding factor of 2 for empty rows/columns.
shortestGalaxyPaths :: String -> [Int]
shortestGalaxyPaths = parseImage >>> analyzeImage 2

-- The second part finds the shortest paths using an expanding factor of 1000000 for empty rows/columns.
hugeExpansionGalaxyPaths :: String -> [Int]
hugeExpansionGalaxyPaths = parseImage >>> analyzeImage 1000000

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
analyzeImage :: Int -> Matrix Quadrant -> [Int]
analyzeImage e m =
  [(x, y) | x <- [1 .. nrows m], y <- [1 .. ncols m], unsafeGet x y m == Galaxy]
    & ( tails
          >>> concatMap
            ( liftA2 map ((,) . head) tailSafe
                >>> map expandedManhattanDistance
            )
      )
  where
    expandedManhattanDistance :: ((Int, Int), (Int, Int)) -> Int
    expandedManhattanDistance ((a, b), (c, d)) =
      abs (a - c)
        + abs (b - d)
        + (e - 1)
          * ( unsafeGet (min a c) (max a c) spaceRowsDiffs
                + unsafeGet (min b d) (max b d) spaceColsDiffs
            )

    spaceRowsDiffs :: Matrix Int
    spaceRowsDiffs = spaceDiffs nrows getRow

    spaceColsDiffs :: Matrix Int
    spaceColsDiffs = spaceDiffs ncols getCol

    spaceDiffs :: (Matrix Quadrant -> Int) -> (Int -> Matrix Quadrant -> Vector Quadrant) -> Matrix Int
    spaceDiffs sf gf = matrix (sf m) (sf m) (uncurry (flip (-)) . both (spacesCounts `unsafeIndex`))
      where
        spacesCounts :: Vector Int
        spacesCounts =
          scanl'
            (\acc x -> acc + if x `elem` spaces then 1 else 0)
            0
            (fromList [1 .. sf m])

        spaces :: [Int]
        spaces = filter (Vector.all (== Space) . (`gf` m)) [1 .. sf m]

------------------------------------------------------------------------------------------------
-- Parsers

parseImage :: String -> Matrix Quadrant
parseImage = fromLists . map (map charToQuadrant) . lines
  where
    charToQuadrant :: Char -> Quadrant
    charToQuadrant c = case c of
      '.' -> Space
      '#' -> Galaxy
      _ -> error "Invalid character in image"