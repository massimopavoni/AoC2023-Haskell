{-# LANGUAGE LambdaCase #-}

module Snowverload (splitComponentSizesProduct) where

import Control.Arrow ((&&&), (>>>))
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap, alter, empty, size, (!))
import Data.List.Split (splitOn)
import Numeric.LinearAlgebra (Matrix, Vector, fromLists, toColumns, toList)
import Numeric.LinearAlgebra.HMatrix (eigSH')
import Safe (headErr, lastDef, tailSafe)

-- This ones was as annoying as it was interesting and rewarding.
-- I started by getting back into some graph theory and asking a past university course mate
-- for insight about graph connectivity and partitioning, given his thesis topic (thanks, Marco).
-- The first solution I implemented was making use of the Karger-Stein randomized algorithm to
-- contract edges until only two super-nodes were left.
-- The problem with this solution is that I don't quite like the "unsubstantial correctness" of it,
-- mainly because of the influence of the RNG starting seed and the necessity of
-- taking into consideration the probability of finding the best min-cut when
-- tweaking the number of vertices under which a contraction phase shouldn't fall.

------------------------------------------------------------------------------------------------
-- Data types

type Vertex = Int

type Graph = HashMap Vertex [Vertex]

------------------------------------------------------------------------------------------------
-- Exports

-- The first (and only) part looks incredibly simple, and it's wonderful knowing it just works.
-- After parsing the input, the answer to our graph partitioning/min-cut question can be found
-- with what's called spectral partitioning/bisection (see spectral decomposition and spectral theorem).
splitComponentSizesProduct :: String -> Int
splitComponentSizesProduct =
  parseWiringDiagram
    >>> eigSH' . graphToLaplacian
    >>> toList . chooseFiedlerVector
    >>> foldl'
      ( \(cs1, cs2) evx ->
          if evx > 0
            then (cs1 + 1, cs2)
            else (cs1, cs2 + 1)
      )
      (0, 0)
    >>> uncurry (*)
  where
    -- The first step is to convert the graph into its Laplacian matrix representation.
    -- The time this takes is negligible compared to the next step operation: eigendecomposition.
    graphToLaplacian :: Graph -> Matrix Double
    graphToLaplacian graph =
      fromLists
        [ [ if i == j
              then fromIntegral $ length (graph ! i)
              else
                if j `elem` (graph ! i) || i `elem` (graph ! j)
                  then -1
                  else 0
            | j <- [1 .. maxV]
          ]
          | i <- [1 .. maxV]
        ]
      where
        maxV :: Int
        maxV = size graph

    -- After hmatrix does its magic, decomposing the Laplacian into its eigenvalues and eigenvectors,
    -- we choose the eigenvector corresponding to the second smallest eigenvalue (Fielder vector).
    -- At this point, the solution is in our hands: the only thing left to do is to count
    -- the number of positive and negative values in the eigenvector, which will give us the sizes
    -- of the two components given by the minimum 2-cut
    -- (which happens to require 3 edges as a result of the tailored input).
    chooseFiedlerVector :: (Vector Double, Matrix Double) -> Vector Double
    chooseFiedlerVector =
      snd
        >>> toColumns
        >>> id &&& (subtract 2 . length)
        >>> uncurry (!!)

------------------------------------------------------------------------------------------------
-- Parsers

-- The input parsing is a bit less straightforward than initially thought;
-- nothing too complicate, but still annoying because of the need to represent undirected edges.
parseWiringDiagram :: String -> Graph
parseWiringDiagram =
  lines
    >>> fst . foldl' parseConnections (empty, empty)
  where
    parseConnections :: (Graph, HashMap String Vertex) -> String -> (Graph, HashMap String Vertex)
    parseConnections (g, vs) l = (updateGraph g, vs')
      where
        updateGraph :: Graph -> Graph
        updateGraph =
          insertEdges (vertex <$> targets) (vertex source)
            >>> flip (foldl' . flip $ insertEdges [vertex source] . vertex) targets
          where
            insertEdges :: [Vertex] -> Vertex -> Graph -> Graph
            insertEdges cs = alter $ \case
              Nothing -> Just cs
              Just es -> Just (cs ++ es)

        splitLine :: [String]
        splitLine = splitOn ": " l

        source :: String
        source = headErr splitLine

        targets :: [String]
        targets = words . lastDef "" $ tailSafe splitLine

        vertex :: String -> Vertex
        vertex = (vs' !)

        vs' :: HashMap String Vertex
        vs' = foldl' insertVertex vs (source : targets)
          where
            insertVertex :: HashMap String Vertex -> String -> HashMap String Vertex
            insertVertex hs = flip (alter addIfMissing) hs
              where
                addIfMissing :: Maybe Vertex -> Maybe Vertex
                addIfMissing Nothing = Just $ size hs + 1
                addIfMissing (Just n) = Just n
