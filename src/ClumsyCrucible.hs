{-# LANGUAGE BangPatterns #-}

module ClumsyCrucible (minimumCrucibleHeatLoss, minimumUltraCrucibleHeatLoss) where

import Algorithm.Search (dijkstra, pruning)
import Control.Category ((>>>))
import Data.Char (digitToInt)
import Data.Matrix (Matrix, fromLists, ncols, nrows, unsafeGet)
import Data.Maybe (fromJust)
import RandomUtils (Direction (..), movePos)

-- I really hated this one. I'm sorry, but I did. And not even because of the problem itself,
-- but because of the final performance I obtained: still working in less than 20 seconds
-- (usually around 10), but just because I didn't want to just implement my own problem-tailored
-- Dijkstra algorithm. I very much prefer solving simple (or complex, but not overly complicated)
-- problems, as opposed to use already ready algorithms and libraries.
-- Still had fun, though.

------------------------------------------------------------------------------------------------
-- Data types

-- I don't actually know if using the bang patterns improved anything throughout the solution.
data Move = M {pos :: !(Int, Int), dir :: !Direction, straight :: !Int}
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
    ( liftA2
        (&&)
        ((>= mins) . straight)
        ((== destination) . pos)
    )
    (M (1, 1) E 0)
  where
    -- The neighbors are very annoying, we have 4 cases:
    -- 1. forced turn if straight moves are over the maximum;
    -- 2. all possible straight moves and turns if straight moves are over the minimum;
    -- 3. only the remaining straight moves if straight moves are under the minimum;
    -- 4. the minimum oves in all directions if this is the first move.
    neighbors :: Move -> [Move]
    neighbors (M p d s)
      | s >= maxs = turnMoves
      | s >= mins = (straightMove <$> [1 .. maxs - s]) ++ turnMoves
      | s > 0 = [straightMove $ mins - s]
      | otherwise = straightMove mins : turnMoves
      where
        straightMove :: Int -> Move
        straightMove a = M (movePos a p d) d (s + a)

        turnMoves :: [Move]
        turnMoves =
          (\d' -> M (movePos mins p d') d' mins)
            <$> if d == S || d == N then [E, W] else [S, N]

    inMatrix :: Move -> Bool
    inMatrix (M (r, c) _ _) = 0 < r && r <= rn && 0 < c && c <= cn

    -- The move cost has to be adapted for use with multiple straight moves at once:
    -- we basically generate the moves in between the two postions and sum the costs.
    moveCost :: Move -> Move -> Int
    moveCost om nm = sum $ flip (uncurry unsafeGet) cm <$> inBetween (pos om) (pos nm)
      where
        inBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
        inBetween (r1, c1) (r2, c2)
          | r1 == r2 = let (minc, maxc) = if c1 < c2 then (c1 + 1, c2) else (c2, c1 - 1) in [(r1, c) | c <- [minc .. maxc]]
          | c1 == c2 = let (minr, maxr) = if r1 < r2 then (r1 + 1, r2) else (r2, r1 - 1) in [(r, c1) | r <- [minr .. maxr]]
          | otherwise = error "Not a straight line"

    destination :: (Int, Int)
    destination = (rn, cn)

    rn :: Int
    rn = nrows cm

    cn :: Int
    cn = ncols cm

------------------------------------------------------------------------------------------------
-- Parsers

parseCity :: String -> Matrix Int
parseCity = fromLists . map (map digitToInt) . lines