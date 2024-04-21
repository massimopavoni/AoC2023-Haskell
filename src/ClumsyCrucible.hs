{-# LANGUAGE BangPatterns #-}

module ClumsyCrucible (minimumCrucibleHeatLoss, minimumUltraCrucibleHeatLoss) where

import Algorithm.Search (dijkstra, pruning)
import Control.Category ((>>>))
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.Matrix (Matrix, fromLists, ncols, nrows, unsafeGet)
import Data.Maybe (fromJust)
import RandomUtils (Direction (..), movePos)

-- I really hated this one. I'm sorry, but I did. And not even because of the problem itself,
-- but because of the final performance I obtained: still working in less than 3 seconds,
-- but just because I didn't want to just implement my own problem-tailored Dijkstra algorithm.
-- I very much prefer solving simple (or complex, but not overly complicated)
-- problems, as opposed to use already ready algorithms and libraries.
-- Still had fun, though, and was able to get rid of counting the straight moves.

------------------------------------------------------------------------------------------------
-- Data types

-- I don't actually know if using bang patterns improved anything throughout the solution.
data Move = Move {pos :: !(Int, Int), dir :: !Direction}
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is using a minimum of 1 and a maximum of 3 straight moves.
minimumCrucibleHeatLoss :: String -> Int
minimumCrucibleHeatLoss =
  parseCity
    >>> fst . fromJust . cityDijkstra 1 3

-- The second part is using a minimum of 4 and a maximum of 10 straight moves.
minimumUltraCrucibleHeatLoss :: String -> Int
minimumUltraCrucibleHeatLoss =
  parseCity
    >>> fst . fromJust . cityDijkstra 4 10

------------------------------------------------------------------------------------------------
-- Functions

-- The cityDijkstra function takes a minimum and a maximum number of straight moves, and a matrix of weights.
-- This would've been much easier if the neighbors weren't so weird, and we could've generated a structure
-- containing edges instead of always going and taking the weights from the matrix.
-- I've thought about still doing that, precomputing all the neighbors and costs, but that's not very easily done,
-- and could just as well end up making the solution even slower.
cityDijkstra :: Int -> Int -> Matrix Int -> Maybe (Int, [Move])
cityDijkstra mins maxs !cm =
  dijkstra
    (neighbors `pruning` (not . inMatrix))
    moveCost
    ((== (rn, cn)) . pos)
    (Move (1, 1) E)
  where
    -- The neighbors are different if it's the first move or not:
    -- either we can only move straight towards east, or south,
    -- or we can only turn, since we always assume moving and then turning
    -- (because we generate the moves always with all the possible entire straight lines,
    -- and leave the choice of the best straight move to the algorithm).
    neighbors :: Move -> [Move]
    neighbors m@(Move p d) =
      moveMoves
        m
        ( if p == (1, 1)
            then
              [S, E]
            else (if d == S || d == N then [E, W] else [S, N])
        )

    moveMoves :: Move -> [Direction] -> [Move]
    moveMoves (Move p _) ds = [Move (movePos s p d') d' | s <- steps, d' <- ds]

    steps :: [Int]
    steps = [mins .. maxs]

    inMatrix :: Move -> Bool
    inMatrix (Move (r, c) _) = 0 < r && r <= rn && 0 < c && c <= cn

    -- The move cost has to be adapted for use with multiple straight moves at once:
    -- we basically generate the moves in between the two postions and sum the costs.
    moveCost :: Move -> Move -> Int
    moveCost om nm = sum $ flip (uncurry unsafeGet) cm <$> inBetween (pos om) (pos nm)
      where
        inBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
        inBetween (r1, c1) (r2, c2)
          | r1 == r2 =
              let (minc, maxc) = bool (c2, c1 - 1) (c1 + 1, c2) (c1 < c2)
               in [(r1, c) | c <- [minc .. maxc]]
          | c1 == c2 =
              let (minr, maxr) = bool (r2, r1 - 1) (r1 + 1, r2) (r1 < r2)
               in [(r, c1) | r <- [minr .. maxr]]
          | otherwise = error "Not a straight line"

    rn :: Int
    rn = nrows cm

    cn :: Int
    cn = ncols cm

------------------------------------------------------------------------------------------------
-- Parsers

parseCity :: String -> Matrix Int
parseCity = fromLists . map (map digitToInt) . lines