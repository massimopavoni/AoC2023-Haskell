{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module PipeMaze (farthestPipeSteps, nestPipesCount) where

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Data.Foldable (find)
import Data.Function ((&))
import Data.Matrix (Matrix, fromLists, ncols, nrows, safeGet, (!))
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)
import RandomUtils (Direction (..), Pos, movePos, oppositeDir)
import Safe (headErr, tailSafe)

-- This problem uses lots of obscure compositions, bear with me.

------------------------------------------------------------------------------------------------
-- Data types

data Pipe = Ground | Start | NS | WE | NW | NE | SW | SE
  deriving (Bounded, Enum, Eq, Show)

data Position = Pos {pos :: Pos, dir :: Direction}
  deriving (Show)

-- We need positions equality only for actual coordinates, not directions.
instance Eq Position where
  (==) :: Position -> Position -> Bool
  (Pos p1 _) == (Pos p2 _) = p1 == p2

------------------------------------------------------------------------------------------------
-- Exports

-- The first part just needs to halve the length of the loop path to find the steps to the farthest pipe.
farthestPipeSteps :: String -> Int
farthestPipeSteps =
  parseMaze
    >>> (`div` 2) . length . walkPipeLoop

-- The second part is instead using the Pick's theorem to find the number of points (pipes) inside the loop:
-- the loop is a polygon on a grid of integer coordinates, so we have that A = I + B/2 - 1,
-- with A area, I internal points and B boundary points. Since we can calculate A with the shoelace formula,
-- we can find I = A - B/2 + 1. The actual composition is slightly different, because we can take out the halving
-- not only from B, but also from the shoelace formula (I = (As - B)/2 + 1, with As being the shoelace summation).
nestPipesCount :: String -> Int
nestPipesCount =
  parseMaze
    >>> walkPipeLoop
    >>> liftA2
      (-)
      (abs . shoelaceSum)
      length
    >>> (+ 1) . (`div` 2)
  where
    shoelaceSum :: [Position] -> Int
    shoelaceSum ps =
      pos <$> ps
        & ( (,map snd shifted) . map fst &&& (,map fst shifted) . map snd
              >>> both (sum . uncurry (zipWith (*)))
              >>> uncurry (-)
          )
      where
        shifted :: [Pos]
        shifted = tailSafe . cycle $ pos <$> ps

------------------------------------------------------------------------------------------------
-- Functions

-- The walkPipeLoop function is a very important one, as it's essentially the one that needs all
-- the above data types, and it's the basis for the two parts' solutions.
-- It essentially uses the until recursion to follow the pipes thrugh the loop starting from the S.
walkPipeLoop :: Matrix Pipe -> [Position]
walkPipeLoop maze =
  until
    ((== Start) . (maze !) . pos . headErr)
    (liftA2 (:) (moveThroughPipe . headErr) id)
    [startPipe, start]
  where
    -- The start pipe is not S itself, but one of the only two possible pipes (the connected ones),
    -- so that we know for sure we're on the big loop. This is also the reason for using
    -- the absolute value of the shoelace sum in nestPipesCount (because we don't know
    -- if we're walking the loop clockwise or counterclockwise).
    startPipe :: Position
    startPipe =
      start
        & ( ((<$> [S ..]) . Pos . pos)
              >>> map (moveThroughPipe . (movePos' <*> dir))
              >>> headErr . filter ((/= (0, 0)) . pos)
          )

    start :: Position
    start =
      (`Pos` N) . fromJust $
        find
          ((== Start) . (maze !))
          [(x, y) | x <- [1 .. nrows maze], y <- [1 .. ncols maze]]

    -- The moveThroughPipe function makes the pipe maze walk possible: it takes a position and returns the next one,
    -- based on what pipe the coordinates identify and from which direction we're coming from.
    moveThroughPipe :: Position -> Position
    moveThroughPipe p@(Pos xy df) = case uncurry safeGet xy maze of
      Nothing -> invalidPos
      Just d -> case d of
        Ground -> invalidPos
        Start -> invalidPos
        NS -> move (S, N)
        WE -> move (E, W)
        NW -> move (S, W)
        NE -> move (S, E)
        SW -> move (N, W)
        SE -> move (E, N)
      where
        invalidPos :: Position
        invalidPos = Pos (0, 0) N

        move :: (Direction, Direction) -> Position
        move (d1, d2)
          | df == d1 = movePos' p d2
          | df == d2 = movePos' p d1
          | otherwise = invalidPos

    movePos' :: Position -> Direction -> Position
    movePos' (Pos p _) dt = (`Pos` oppositeDir dt) $ movePos 1 p dt

------------------------------------------------------------------------------------------------
-- Parsers

-- I finally avoided using megaparsec, ahah.
parseMaze :: String -> Matrix Pipe
parseMaze = fromLists . map (map charToPipe) . lines
  where
    charToPipe :: Char -> Pipe
    charToPipe = fromJust . (`lookup` zip ".S|-7FJL" [Ground ..])
