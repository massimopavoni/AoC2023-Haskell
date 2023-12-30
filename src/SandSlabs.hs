{-# LANGUAGE TupleSections #-}

module SandSlabs (safeBricksCount, unsafeBrickFallsCount) where

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Data.Array (array, bounds, (!))
import qualified Data.Array as DArr (elems)
import Data.Foldable (foldl')
import Data.Graph (Graph, Vertex, reachable, transposeG)
import Data.HashMap.Strict (HashMap, foldlWithKey', keys, size)
import qualified Data.HashMap.Strict as HsMS (empty, filter, foldl', insert)
import Data.HashSet (HashSet, fromList)
import qualified Data.HashSet as HSet (size, toList)
import Data.List (sort)
import Data.List.Split (splitOn)

------------------------------------------------------------------------------------------------
-- Data types

-- The Brick type has z coordinates first, so that we can sort bricks by height.
data Brick = Brick
  { firstZ :: Int,
    secondZ :: Int,
    firstX :: Int,
    secondX :: Int,
    firstY :: Int,
    secondY :: Int
  }
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is easy (after figuring out the falling and graph making stuff),
-- as I can just count the unsafe bricks and subtract them from the total number of nodes.
safeBricksCount :: String -> Int
safeBricksCount =
  parseBricksSnapshot
    >>> fallBricksIntoGraph
    >>> snd . bounds &&& HSet.size . findUnsafeBricks
    >>> uncurry (-)

-- The second part is slightly more complicated: we have to count the number of bricks
-- that would fall by removing every unsafe brick, one at a time.
unsafeBrickFallsCount :: String -> Int
unsafeBrickFallsCount =
  parseBricksSnapshot
    >>> fallBricksIntoGraph
    >>> id &&& HSet.toList . findUnsafeBricks
    >>> uncurry countFalls
    >>> sum
  where
    -- To do that, we use the transposed graph (I'm not completely sure that transposing
    -- the entire graph is the best way, once fixed my graph approach) to get the reachable nodes
    -- from each unsafe brick, then we check the children of the original graph
    -- to make sure that all the supporting bricks have already fallen.
    -- We use the sorted tail of the reachable nodes because:
    -- 1. we have to process the bricks "in order" so that when we check the next brick,
    -- we can be sure that if the supporting bricks haven't already fallen,
    -- there are other supporting bricks that wouldn't fall because of
    -- the one being currently analyzed (meaning we have to stop going up that branch);
    -- 2. the first node is the unsafe brick itself.
    countFalls :: Graph -> [Vertex] -> [Int]
    countFalls graph =
      let transposedGraph = transposeG graph
       in map
            ( pure &&& sort . tail . reachable transposedGraph
                >>> length . uncurry fallingBricks
            )
      where
        fallingBricks :: [Vertex] -> [Vertex] -> [Vertex]
        fallingBricks fvs [] = init fvs
        fallingBricks fvs (rv : rvs) =
          fallingBricks
            ( if all (`elem` fvs) (graph ! rv)
                then rv : fvs
                else fvs
            )
            rvs

------------------------------------------------------------------------------------------------
-- Functions

-- This function is the main part, as once we have the bricks support dependencites organized in a graph,
-- we can easily solve the two parts of the puzzle.
-- The idea is to let the bricks fall until they reach a stable position,
-- memorizing the supports of each brick in the meantime, to then finally directly create the Graph Array.
fallBricksIntoGraph :: [Brick] -> Graph
fallBricksIntoGraph =
  sort
    >>> zip [1 ..]
    >>> foldl' fallingBrick HsMS.empty
    >>> liftA2 array ((1,) . size) (foldlWithKey' (\a k v -> (k, snd v) : a) [])
  where
    -- A falling brick can be in one of three states:
    -- 1. it's below the ground, it is not supported by any other brick, so we bring it up to level 1;
    -- 2. it's not intersecting any other brick, so we have to let it fall;
    -- 3. it's intersecting other bricks, so we can bring it back up by one unit and let it rest on its supports.
    fallingBrick :: HashMap Vertex (Brick, [Vertex]) -> (Vertex, Brick) -> HashMap Vertex (Brick, [Vertex])
    fallingBrick tower (v, b@(Brick fz cz _ _ _ _))
      | fz == 0 = HsMS.insert v (b {firstZ = 1, secondZ = cz + 1}, []) tower
      | null intersections =
          fallingBrick
            tower
            (v, b {firstZ = fz - maximumDeltaZ, secondZ = cz - maximumDeltaZ})
      | otherwise = HsMS.insert v (b {firstZ = fz + 1, secondZ = cz + 1}, intersections) tower
      where
        intersections :: [Vertex]
        intersections = keys $ HsMS.filter (bricksIntersect b . fst) tower
          where
            bricksIntersect :: Brick -> Brick -> Bool
            bricksIntersect (Brick z1 _ x1 x2 y1 y2) (Brick _ z2 x3 x4 y3 y4) =
              z1 == z2 && overlap x1 x2 x3 x4 && overlap y1 y2 y3 y4
              where
                overlap :: Int -> Int -> Int -> Int -> Bool
                overlap a1 a2 b1 b2 = max a1 b1 <= min a2 b2

        -- This is the maximum steps we can let the current brick fall,
        -- calculated by subtracting the maximum height of the current tower from the current brick's z coordinate.
        -- (I make use of the Hash Map foldl' instead of using maximum on the list of elements
        -- because that approach would imply traversing the list twice.)
        maximumDeltaZ :: Int
        maximumDeltaZ =
          fz
            - HsMS.foldl'
              ( \acc b' ->
                  let z2' = secondZ (fst b')
                   in if z2' < fz
                        then max acc z2'
                        else acc
              )
              0
              tower

-- To find the bricks that we cannot safely remove,
-- we get the single child of every node with only one child.
findUnsafeBricks :: Graph -> HashSet Vertex
findUnsafeBricks =
  DArr.elems
    >>> filter ((== 1) . length)
    >>> concat
    >>> fromList

------------------------------------------------------------------------------------------------
-- Parsers

parseBricksSnapshot :: String -> [Brick]
parseBricksSnapshot =
  lines
    >>> map
      ( splitOn "~"
          >>> concatMap (splitOn ",")
          >>> map read
          >>> listToBrick
      )
  where
    listToBrick :: [Int] -> Brick
    listToBrick [x1, y1, z1, x2, y2, z2] = Brick z1 z2 x1 x2 y1 y2
    listToBrick _ = error "Invalid brick"