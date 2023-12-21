module TheFloorWillBeLava (energizedTilesCount, energizedTilesCountAllStarts) where

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Control.Parallel.Strategies (parMap, rseq)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap, insertWith, singleton, size)
import qualified Data.HashMap.Strict as HsMS (lookup)
import Data.Matrix (Matrix, fromLists, ncols, nrows, safeGet)
import Data.Maybe (fromJust)
import RandomUtils (Direction (..), movePos)

-- I was proud of not giving up and look at some people's solutions to get inspiration for this one,
-- managed to stop trying what I was doing, which was crazy (I think it was some sort of DFS
-- and hashset unions, while approaching with BFS and hashmaps is much better), and just start again.
-- It led to a nice solution, even though the second part is kind of brute-forcing its way through
-- the multiple starting positions and directions: I might get back on this in the future to try
-- and slap DP (?) on it, in such a way that I can share information between the multiple function applications.
-- (Also, I just gotta say I'm so fed up with all of these grids problems, dammit.)

------------------------------------------------------------------------------------------------
-- Data types

data Tile = Empty | Mirror | AntiMirror | ColSplit | RowSplit
  deriving (Bounded, Enum, Eq, Show)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part just applies the followLightBeams function with the default starting position and direction.
energizedTilesCount :: String -> Int
energizedTilesCount =
  let start = ((1, 1), E)
   in parseContraption
        >>> followLightBeams (liftA2 singleton fst (pure . snd) start) [start]
        >>> size

-- The second part instead has to use all the possible starting positions along the contraption's borders.
energizedTilesCountAllStarts :: String -> [Int]
energizedTilesCountAllStarts =
  parseContraption
    >>> (starts &&& id)
    >>> \(ss, tm) ->
      parMap
        rseq
        ( liftA2 singleton fst (pure . snd) &&& pure
            >>> flip (uncurry followLightBeams) tm
            >>> size
        )
        ss
  where
    -- Just 4 list comprehensions, nothing to see here.
    starts :: Matrix Tile -> [((Int, Int), Direction)]
    starts tm =
      let (nrs, ncs) = (nrows tm, ncols tm)
          (rs, cs) = ([1 .. nrs], [1 .. ncs])
       in [((1, y), S) | y <- cs]
            ++ [((x, 1), E) | x <- rs]
            ++ [((nrs, y), N) | y <- cs]
            ++ [((x, ncs), W) | x <- rs]

------------------------------------------------------------------------------------------------
-- Functions

-- The followLightBeams is the core, of course, and it uses a HashMap to keep track of the visited tiles,
-- while accepting a list of the last tiles visited and to use for the next recursion.
-- The HashMap values are Direction lists, because we have to check if the tile we're visiting
-- was already encountered, but with a different direction, in which case we have to pass through it again,
-- because it might lead to more unvisited tiles and therefore a higher count.
followLightBeams :: HashMap (Int, Int) [Direction] -> [((Int, Int), Direction)] -> Matrix Tile -> HashMap (Int, Int) [Direction]
followLightBeams ehm [] _ = ehm
followLightBeams ehm ts tm = followLightBeams' ehm ts
  where
    followLightBeams' :: HashMap (Int, Int) [Direction] -> [((Int, Int), Direction)] -> HashMap (Int, Int) [Direction]
    followLightBeams' shm cts =
      followLightBeams
        (foldl' (\hm (pos, dir) -> insertWith (++) pos [dir] hm) shm nextTiles)
        nextTiles
        tm
      where
        nextTiles :: [((Int, Int), Direction)]
        nextTiles = concatMap (filter unseen . uncurry nextTiles') cts
          where
            -- It's also important to notice that other than not passing through the tiles we've visited
            -- with the same direction, we don't have to visit vertical or horizontal splitters again ever,
            -- because no matter which direction we're going towards, we're sure we already visited
            -- everything through a splitter (if you just think about it for a bit, you'll quickly understand
            -- that loops and maybe other other special cases are not a problem thanks to this).
            unseen :: ((Int, Int), Direction) -> Bool
            unseen (pos, dir) = case uncurry safeGet pos tm of
              Nothing -> False
              Just ct -> case HsMS.lookup pos ehm of
                Just ds -> ct /= ColSplit && ct /= RowSplit && dir `notElem` ds
                Nothing -> True

            nextTiles' :: (Int, Int) -> Direction -> [((Int, Int), Direction)]
            nextTiles' pos dir =
              (movePos 1 pos &&& id) <$> case uncurry safeGet pos tm of
                Nothing -> []
                Just Empty -> [dir]
                Just Mirror -> [[W, N, E, S] !! fromEnum dir]
                Just AntiMirror -> [[E, S, W, N] !! fromEnum dir]
                Just ColSplit -> if dir `elem` [E, W] then [S, N] else [dir]
                Just RowSplit -> if dir `elem` [S, N] then [E, W] else [dir]

------------------------------------------------------------------------------------------------
-- Parsers

parseContraption :: String -> Matrix Tile
parseContraption = fromLists . map (map charToTile) . lines
  where
    charToTile :: Char -> Tile
    charToTile = fromJust . (`lookup` zip "./\\|-" [Empty ..])