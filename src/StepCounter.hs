module StepCounter (gardenReachablePlotsCount, infiniteGardenReachablePlotsCount) where

import Control.Arrow (first, second, (***))
import Control.Category ((>>>))
import Data.Foldable (find, foldMap')
import Data.Function ((&))
import Data.Matrix (fromLists, ncols, nrows, (!))
import qualified Data.Matrix as DMat (Matrix)
import Data.Maybe (fromJust)
import Data.Set (Set, empty, fromList, notMember, singleton, size)
import Data.Tuple.Extra (both)
import Numeric.LinearAlgebra (linearSolve)
import Numeric.LinearAlgebra.Data (asColumn, flatten, toList, vector, (><))
import qualified Numeric.LinearAlgebra.Data as LMat (Matrix)
import RandomUtils (Pos)
import Safe (headErr)

-- This is not a fair problem: it had very specific properties that made the second part not fun.
-- What I am thinking is that crafting 25 puzzles this well thought out and maintaining a level
-- that's good for most of the people who try and solve them is not easy,
-- and sometimes one (the craftsman) can have some difficulties and not really be able
-- to avoid some of the things I would consider pitfalls,
-- like slightly overfitting some parts of the puzzle to the peculiar input.

---------------------------------------------------------------------------------------------------
-- Exports

-- The first part is easy enough, since it just involves a breadth-first search.
gardenReachablePlotsCount :: String -> Int
gardenReachablePlotsCount =
    fromLists . lines
        >>> findReachablePlots 64
        >>> headErr

-- The second part is the annoying thing I mentioned above.
-- I don't care how ingenious the puzzle can be, it is not fun to solve it if it requires you
-- to analyze the properties of the special input and find a random quadratic progression
-- in the growth of the number of reachable plots.
-- That being said, I read other people's thoughts on it and analyzed the input on my own,
-- plotting the relationship between the number of steps and the number of reachable plots,
-- realizing the actual quadratic curve and then implementing a solution using
-- the hmatrix package to solve the linear system for the coefficients
-- (exploring the package was one of the few fun parts, at least).
-- We even have to choose specific steps to analyze, since the quadratic function works well for numbers
-- that are congruent to 65 modulo 131, since the input has a width and height of 131
-- and the edge of the garden tile can be reached in 65 steps without encountering any rocks.
-- I didn't try making the choice of the sample steps and step points depend on the input size,
-- but I still have serious doubts that the solution would work for any other type of input,
-- especially because of the visible empty garden plots diamond
-- (it's just my guess, but that might be part of the reason why the growth follows a pattern
-- that can be used to make accurate predictions on the answer for different step numbers),
-- and because of the precise number of steps mentioned in the puzzle description
-- (26501365 is indeed congruent to 65 modulo 131, even though maybe this property only makes
-- the result precise for those numbers and an approximation to be rounded for different values).
infiniteGardenReachablePlotsCount :: String -> Int
infiniteGardenReachablePlotsCount =
    fromLists . lines
        >>> findReachablePlots sampleSteps
        >>> (<$> map (sampleSteps -) stepPoints) . (!!)
        >>> asColumn . vector . map fromIntegral
        >>> linearSolve a
        >>> toList . flatten . fromJust
        >>> zipWith (*) (variableQuadratic 26501365)
        >>> round . sum
  where
    sampleSteps :: Int
    sampleSteps = 327

    stepPoints :: [Int]
    stepPoints = [65, 196, 327]

    a :: LMat.Matrix Double
    a = (3 >< 3) $ concatMap variableQuadratic stepPoints

    variableQuadratic :: Int -> [Double]
    variableQuadratic = (<$> ([2, 1, 0] :: [Int])) . (^) . fromIntegral

---------------------------------------------------------------------------------------------------
-- Functions

-- The breadth-first-search itself is straightforward enough.
-- The only quirks are saving the previous positions to avoid visiting them again
-- (and making use of a set for it, because we can simply use their Monoid instance with foldMap',
-- which is pretty nice for the knowledge I have, not still having studied the type classes
-- that take from Algebra and Category Theory).
findReachablePlots :: Int -> DMat.Matrix Char -> [Int]
findReachablePlots steps garden = bfsGardenPlotsCount (steps, singleton start) empty [0, 0]
  where
    start :: Pos
    start =
        fromJust $
            find
                ((== 'S') . (garden !))
                [(x, y) | x <- [1 .. rowsCount], y <- [1 .. colsCount]]

    bfsGardenPlotsCount :: (Int, Set Pos) -> Set Pos -> [Int] -> [Int]
    bfsGardenPlotsCount (ss, cp) pp cs
        | ss == 0 = cs'
        | otherwise = bfsGardenPlotsCount (ss - 1, foldMap' nextPositions cp) cp cs'
      where
        nextPositions :: Pos -> Set Pos
        nextPositions =
            (&)
                >>> (<$> [first (subtract 1), second (subtract 1), first (+ 1), second (+ 1)])
                >>> filter
                    ( liftA2
                        (&&)
                        (`notMember` pp)
                        ( both (subtract 1)
                            >>> (`mod` rowsCount) *** (`mod` colsCount)
                            >>> both (+ 1)
                            >>> (/= '#') . (garden !)
                        )
                    )
                >>> fromList

        cs' :: [Int]
        cs' = (cs !! 1 + size cp) : cs

    rowsCount :: Int
    rowsCount = nrows garden

    colsCount :: Int
    colsCount = ncols garden
