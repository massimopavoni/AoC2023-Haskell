{-# LANGUAGE BangPatterns #-}

module ClumsyCrucible (minimumCrucibleHeatLoss, minimumUltraCrucibleHeatLoss) where

import Algorithm.Search (aStar, pruning)
import Control.Category ((>>>))
import Data.Char (digitToInt)
import Data.Matrix (Matrix, fromLists, ncols, nrows, unsafeGet)
import Data.Maybe (fromJust)
import RandomUtils (Direction (..), manhattanDistance, movePos)

-- exampleCity :: String
-- exampleCity =
--   "2413432311323\n\
--   \3215453535623\n\
--   \3255245654254\n\
--   \3446585845452\n\
--   \4546657867536\n\
--   \1438598798454\n\
--   \4457876987766\n\
--   \3637877979653\n\
--   \4654967986887\n\
--   \4564679986453\n\
--   \1224686865563\n\
--   \2546548887735\n\
--   \4322674655533\n"

------------------------------------------------------------------------------------------------
-- Data types

data Move = M {pos :: !(Int, Int), dir :: !Direction, straight :: !Int}
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------------------------
-- Exports

minimumCrucibleHeatLoss :: String -> Int
minimumCrucibleHeatLoss =
  parseCity
    >>> fst . fromJust . cityAStar 0 3

minimumUltraCrucibleHeatLoss :: String -> Int
minimumUltraCrucibleHeatLoss =
  parseCity
    >>> fst . fromJust . cityAStar 4 10

------------------------------------------------------------------------------------------------
-- Functions

cityAStar :: Int -> Int -> Matrix Int -> Maybe (Int, [Move])
cityAStar mins maxs !cm =
  aStar
    (neighbors `pruning` (not . inMatrix))
    moveCost
    distanceHeuristic
    ( liftA2
        (&&)
        ((>= mins) . straight)
        ((== destination) . pos)
    )
    (M (1, 1) E 0)
  where
    neighbors :: Move -> [Move]
    neighbors (M p d s)
      | s == 0 = straightMove : turnMoves
      | s < mins = [straightMove]
      | s < maxs = straightMove : turnMoves
      | otherwise = turnMoves
      where
        straightMove :: Move
        straightMove = M (movePos p d) d (s + 1)

        turnMoves :: [Move]
        turnMoves =
          (\d' -> M (movePos p d') d' 1)
            <$> if d == S || d == N then [E, W] else [S, N]

    inMatrix :: Move -> Bool
    inMatrix (M (r, c) _ _) = 0 < r && r <= rn && 0 < c && c <= cn

    moveCost :: Move -> Move -> Int
    moveCost _ m = uncurry unsafeGet (pos m) cm

    distanceHeuristic :: Move -> Int
    distanceHeuristic = manhattanDistance destination . pos

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