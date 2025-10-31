{-# LANGUAGE TupleSections #-}

module ALongWalk (walkLongestHike, walkLongestDryHike) where

import Control.Applicative (liftA3)
import Control.Category ((>>>))
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HsMS (alter, empty, insert, toList, union, (!))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HSet (empty, insert, member, singleton)
import Data.List (partition)
import Data.Matrix (Matrix, fromLists, getRow, nrows)
import qualified Data.Matrix as DMat ((!))
import Data.Maybe (fromJust)
import Data.Vector (elemIndex)
import RandomUtils (Pos)

---------------------------------------------------------------------------------------------------
-- Data types

-- We use a directed graph for input "compression",
-- with edges weights representing distance between forest crossroads.
type Graph = HashMap Pos [(Pos, Int)]

data Forest = Forest Pos Pos Graph
  deriving (Show)

---------------------------------------------------------------------------------------------------
-- Exports

-- The first part considers the slopes as icy and not walkable the opposite direction.
walkLongestHike :: String -> Int
walkLongestHike = findLongestHike False

-- The second part considers the slopes as dry enough to walk in any direction, allowing for longer hikes.
walkLongestDryHike :: String -> Int
walkLongestDryHike = findLongestHike True

---------------------------------------------------------------------------------------------------
-- Functions

-- The solution strategy can be expressed with just a single function, although it's quite complicated.
findLongestHike :: Bool -> String -> Int
findLongestHike dry =
  fromLists . lines
    >>> matrixToForest
    >>> (if dry then pruneDeadEnds else id)
    >>> findLongest
  where
    -- The solution itself is just a brute-force DFS, because of the NP-hard nature of the longest path problem
    -- on what's basically actually an undirected graph in this case
    -- (and there seem to be no peculiar properties the graph has that would allow for a more efficient solution).
    findLongest :: Forest -> Int
    findLongest (Forest entrance exit graph) = findLongest' HSet.empty 0 entrance
      where
        findLongest' :: HashSet Pos -> Int -> Pos -> Int
        findLongest' visited distance current
          | current == exit = distance
          | otherwise = longest
          where
            longest :: Int
            longest = maximum (0 : map (\(c, d) -> findLongest' seen' (distance + d) c) neighbors)
              where
                seen' :: HashSet Pos
                seen' = HSet.insert current visited

                neighbors :: [(Pos, Int)]
                neighbors = [(p, d') | (p, d') <- graph HsMS.! current, not $ HSet.member p seen']

    -- One of the things to note here is the optimization that the pruneDeadEnds function provides:
    -- this simplifies the graph by removing perimeter connections that would lead to getting stuck within
    -- the already tracked path, but since this makes sense only for the second part we skip it for the first.
    pruneDeadEnds :: Forest -> Forest
    pruneDeadEnds (Forest entrance exit graph) =
      Forest entrance exit $
        HsMS.union (perimeter HSet.empty HsMS.empty [(exit, 0)]) graph
      where
        perimeter :: HashSet Pos -> Graph -> [(Pos, Int)] -> Graph
        perimeter _ _ [] = error "No nodes to visit"
        perimeter vs g ((n, _) : ns)
          | n == entrance = g
          | otherwise = perimeter (HSet.insert n vs) g' ns'
          where
            g' :: Graph
            g' = HsMS.insert n (fst nodesPartitions) g

            ns' :: [(Pos, Int)]
            ns' = ns ++ snd nodesPartitions

            -- A node is considered good to keep if it's the exit node itself (from which we start the pruning),
            -- if it's a permiter node we've already visited, or if it has more than 3 neighbors (center crossroads).
            -- The remaining case includes all the perimeter node connections that would lead to getting stuck
            -- (these are essentially the perimeter nodes left of or above the current one).
            nodesPartitions :: ([(Pos, Int)], [(Pos, Int)])
            nodesPartitions =
              graph HsMS.! n
                & partition
                  ( fst
                      >>> liftA3
                        (\a b c -> a || b || c)
                        (== exit)
                        (`HSet.member` vs)
                        ((> 3) . length . (graph HsMS.!))
                  )

    -- The matrixToForest function is the least clean, and it's the most important one
    -- (together with pruning dead ends for the second part) for speeding up the longest path search.
    -- The plan is to "compress" the input matrix into a graph of crossroads,
    -- where the edge weights represent the length of the connection paths between crossroads.
    matrixToForest :: Matrix Char -> Forest
    matrixToForest m = Forest entrance exit $ findCrossroadDistances (HSet.singleton entrance) HsMS.empty [entrance]
      where
        entrance :: Pos
        entrance = (1, 1 + fromJust (elemIndex '.' $ getRow 1 m))

        exit :: Pos
        exit =
          let rn = nrows m
           in (rn, 1 + fromJust (elemIndex '.' $ getRow rn m))

        findCrossroadDistances :: HashSet Pos -> Graph -> [Pos] -> Graph
        findCrossroadDistances _ graph [] = graph
        findCrossroadDistances visited graph (n : next) =
          findCrossroadDistances
            (foldl' (flip HSet.insert) visited crossroads)
            (HsMS.insert n nextCrossroads graph)
            (next ++ crossroads)
          where
            crossroads :: [Pos]
            crossroads = foldl' (\cs (nc, _) -> if HSet.member nc visited then cs else nc : cs) [] nextCrossroads

            nextCrossroads :: [(Pos, Int)]
            nextCrossroads = HsMS.toList $ findNextCrossroads (HSet.singleton n) [(n, 0)] HsMS.empty
              where
                findNextCrossroads :: HashSet Pos -> [(Pos, Int)] -> HashMap Pos Int -> HashMap Pos Int
                findNextCrossroads _ [] found = found
                findNextCrossroads seen ((c@(y, x), d) : ns) found
                  | d > 0 && isCrossroad =
                      findNextCrossroads
                        seen
                        ns
                        (HsMS.alter (Just . maybe d (max d)) c found)
                  | otherwise =
                      findNextCrossroads
                        (foldl' (flip HSet.insert) seen crossroadNeighbors)
                        (ns ++ map (,d + 1) crossroadNeighbors)
                        found
                  where
                    -- Entrance and exit are crossroads, as well as any node with more than 2 neighboring crossroads.
                    isCrossroad :: Bool
                    isCrossroad = c == entrance || c == exit || length neighbors > 2

                    crossroadNeighbors :: [Pos]
                    crossroadNeighbors = filter (not . (`HSet.member` seen)) neighbors

                    -- Neighbors are filtered based on dry or icy conditions, as well as walls and entrance/exit special cases.
                    neighbors :: [Pos]
                    neighbors
                      | c == entrance = [(y + 1, x)]
                      | c == exit = [(y - 1, x)]
                      | otherwise =
                          let allNeighbors = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
                           in filter ((/= '#') . (m DMat.!)) $
                                if dry
                                  then allNeighbors
                                  else case m DMat.! c of
                                    '>' -> [(y, x + 1)]
                                    'v' -> [(y + 1, x)]
                                    _ -> allNeighbors