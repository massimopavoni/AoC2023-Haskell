{-# LANGUAGE LambdaCase #-}

module Snowverload (splitComponentSizes) where

import Control.Arrow ((&&&), (>>>))
import Data.Foldable (foldl', minimumBy)
import Data.Function (on, (&))
import Data.HashMap.Strict (HashMap, adjust, alter, delete, elems, empty, insert, keys, size, (!))
import qualified Data.HashMap.Strict as HsMS (map)
import Data.List.Split (splitOn)
import Safe (headErr, lastDef, tailSafe)
import System.Random (Random (randomR), StdGen, mkStdGen)

------------------------------------------------------------------------------------------------
-- Data types

type Vertex = Int

type Graph = HashMap Vertex [Vertex]

------------------------------------------------------------------------------------------------
-- Exports

splitComponentSizes :: Int -> String -> (Int, Int)
splitComponentSizes minCut =
  parseWiringDiagram
    >>> fastMinCut (mkStdGen 13)
    >>> snd . fst
    >>> elems
    >>> headErr &&& last
  where
    fastMinCut :: StdGen -> (Graph, HashMap Vertex Int) -> ((Graph, HashMap Vertex Int), StdGen)
    fastMinCut rng (graph, nodeSizes)
      | edgesCount graph <= minCut * 2 = ((graph, nodeSizes), rng)
      | size graph <= 6 = contract (graph, nodeSizes) rng 2
      | otherwise =
          let t = ceiling (1 + fromIntegral (size graph) / 2 :: Double)

              (gns1, rng') = contract (graph, nodeSizes) rng t

              (gns2, rng'') = contract (graph, nodeSizes) rng' t

              r1@(_, rng''') = fastMinCut rng'' gns1

              r2@(_, _) = fastMinCut rng''' gns2
           in minimumBy (compare `on` (edgesCount . fst . fst)) [r1, r2]
      where
        edgesCount :: Graph -> Int
        edgesCount = sum . map length . elems

        contract :: (Graph, HashMap Vertex Int) -> StdGen -> Int -> ((Graph, HashMap Vertex Int), StdGen)
        contract (graphC, nodeSizesC) rngC maxVertices =
          let vertices = keys graphC

              (uIndex, rngC') = randomR (0, length vertices - 1) rngC

              u = vertices !! uIndex

              neighbors = graphC ! u

              (vIndex, rngC'') = randomR (0, length neighbors - 1) rngC'

              v = neighbors !! vIndex
           in if size graphC <= maxVertices
                then ((graphC, nodeSizesC), rngC'')
                else contract (contractEdge (graphC, nodeSizesC) (u, v)) rngC'' maxVertices
          where
            contractEdge :: (Graph, HashMap Vertex Int) -> (Vertex, Vertex) -> (Graph, HashMap Vertex Int)
            contractEdge (g, ns) (u, v) =
              ( insert u (filter (/= v) (g ! u) ++ filter (/= u) (g ! v)) g
                  & HsMS.map (map (\x -> if x == v then u else x))
                    . delete v,
                adjust (+ (ns ! v)) u $ delete v ns
              )

------------------------------------------------------------------------------------------------
-- Parsers

parseWiringDiagram :: String -> (Graph, HashMap Vertex Int)
parseWiringDiagram =
  lines
    >>> fst . foldl' parseConnections (empty, empty)
    >>> id &&& HsMS.map (const 1)
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
