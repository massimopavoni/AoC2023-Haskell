module Aplenty (acceptedPartRatings, acceptedPartRatingCombinations) where

import Control.Applicative (liftA3)
import Control.Arrow (second, (***))
import Control.Category ((>>>))
import Data.Bool (bool)
import Data.Function ((&))
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap, fromList, (!))
import Data.List.Extra (breakOn, find)
import Data.Maybe (fromJust, mapMaybe)
import Data.Tuple.Extra (both, uncurry3)
import RandomUtils (Parser, parseInput)
import Text.Megaparsec (between, choice, oneOf, optional, sepBy, some, takeWhileP, try, (<|>))
import Text.Megaparsec.Char (char, letterChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

-- This is one of my ugliest codebases, I'm sorry.
-- (There are still some pretty nice functions and compositions, tho.)

------------------------------------------------------------------------------------------------
-- Data types

-- Tried using lenses with this one, didn't manage, will study the library at some point.
data Part a = Part {xValue :: a, mValue :: a, aValue :: a, sValue :: a}
  deriving (Show)

data WorkflowRange = WfRange {xmas :: Char, condition :: Ordering, branch :: Int, next :: String}
  deriving (Show)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is easy enough: the number of parts to sort is not huge,
-- and we can simply move them through the workflows.
-- A nice use of the reverse application is used here for getting the part values,
-- as well as the now loved and probably overused applicative lifting
-- and a rather obscure until function (it just gets the next workflow thanks to
-- the lazy evaluation of the find function over the list of mapped workflow conditions).
acceptedPartRatings :: String -> [Int]
acceptedPartRatings =
  parseWorkflowsAndParts
    >>> uncurry partsThroughWorkflows
    >>> concatMap ((<$> partValueFunctions) . (&))
  where
    partsThroughWorkflows :: HashMap String [Part a -> String] -> [Part a] -> [Part a]
    partsThroughWorkflows hm =
      mapMaybe
        ( \p ->
            bool
              Nothing
              (Just p)
              ( until
                  (liftA2 (||) (== "A") (== "R"))
                  ( (hm !)
                      >>> map ($ p)
                      >>> fromJust . find (not . null)
                  )
                  "in"
                  == "A"
              )
        )

-- The second part is the main monster, and there's a lot to unpack:
-- once we have all the accepted ranges for part ratings, we just multiply those to get all possible combinations...
-- ...but how to we get the ranges?
acceptedPartRatingCombinations :: String -> [Int]
acceptedPartRatingCombinations =
  parseWorkflows
    >>> rangesThroughWorkflows
    >>> map (map ((+ 1) . abs . uncurry (-)) . (<$> partValueFunctions) . (&))
    >>> map product
  where
    -- We start from when input workflow and with all possible rating ranges.
    rangesThroughWorkflows :: HashMap String [WorkflowRange] -> [Part (Int, Int)]
    rangesThroughWorkflows hm = rangesThroughWorkflows' "in" $ Part (1, 4000) (1, 4000) (1, 4000) (1, 4000)
      where
        -- The real recursive function has three cases:
        -- 1. we're in an accepting state, so we return the current ranges;
        -- 2. we're in a rejecting state, so we return an empty list;
        -- 3. we're in a workflow that still has conditions;
        -- so we have to follow the workflow jumps until any of the previous cases is met.
        rangesThroughWorkflows' :: String -> Part (Int, Int) -> [Part (Int, Int)]
        rangesThroughWorkflows' "A" ranges = [ranges]
        rangesThroughWorkflows' "R" _ = []
        rangesThroughWorkflows' state ranges = followAccept (hm ! state) ranges
          where
            -- The followAccept function is another (mutually) recursive one that rules the previous case 3.
            -- Either we only have one remaining condition, in which case it's an unconditional jump,
            -- or we have to analyze the current condition and potentially split the ranges.
            followAccept :: [WorkflowRange] -> Part (Int, Int) -> [Part (Int, Int)]
            followAccept [WfRange _ _ _ n] rs = rangesThroughWorkflows' n rs
            followAccept ((WfRange wx c b n) : wrs) p@(Part x m a s) = case wx of
              'x' -> processRange x
              'm' -> processRange m
              'a' -> processRange a
              's' -> processRange s
              _ -> error "Invalid xmas"
              where
                -- The splitting cases are:
                -- 1. the entire range respects the condition (either appearing before or after the branching number),
                -- in which case we know for sure we're swiching workflow, without continuing on the current one,
                -- but maintaining the current range;
                -- 2. the range doesn't respect the condition, meaning we'll just continue on the current workflow,
                -- not branching onto another, and maintaining the current range;
                -- 3. the range is split by the branching number (with the auxiliary splitRange function),
                -- and we have to follow both branches, one ending up on a new workflow, the other continuing on the current one.
                processRange :: (Int, Int) -> [Part (Int, Int)]
                processRange range = case splitRange c range of
                  (_, (0, 0)) -> rangesThroughWorkflows' n p
                  ((0, 0), _) -> followAccept wrs p
                  (rr1, rr2) -> rangesThroughWorkflows' n (updatePart wx rr1) ++ followAccept wrs (updatePart wx rr2)
                  where
                    splitRange :: Ordering -> (Int, Int) -> ((Int, Int), (Int, Int))
                    splitRange LT r@(r1, r2)
                      | r1 < b = if r2 < b then (r, (0, 0)) else ((r1, b - 1), (b, r2))
                      | otherwise = ((0, 0), r)
                    splitRange GT r@(r1, r2)
                      | r2 > b = if r1 > b then (r, (0, 0)) else ((b + 1, r2), (r1, b))
                      | otherwise = ((0, 0), r)
                    splitRange _ _ = error "Invalid range split"

                    updatePart :: Char -> (Int, Int) -> Part (Int, Int)
                    updatePart 'x' rr = p {xValue = rr}
                    updatePart 'm' rr = p {mValue = rr}
                    updatePart 'a' rr = p {aValue = rr}
                    updatePart 's' rr = p {sValue = rr}
                    updatePart _ _ = error "Invalid xmas"
            followAccept _ _ = error "Invalid workflow"

------------------------------------------------------------------------------------------------
-- Functions

partValueFunctions :: [Part a -> a]
partValueFunctions = [xValue, mValue, aValue, sValue]

------------------------------------------------------------------------------------------------
-- Parsers

-- The parsing functions are for sure the unsung heroes of the solutions,
-- but I'm not going to comment them in detail, mainly because I'm convinced that compared to
-- the rest of the functions they're straightforward and declarative enough to be understood easily.
parseWorkflowsAndParts :: String -> (HashMap String [Part Int -> String], [Part Int])
parseWorkflowsAndParts =
  breakOn "\n\n"
    >>> second (drop 2)
    >>> both lines
    >>> fromList . map (parseInput workflowParser id) *** map (parseInput partParser id)
  where
    workflowParser :: Parser (String, [Part Int -> String])
    workflowParser = do
      name <- takeWhileP Nothing (/= '{')
      functions <- betweenCurly $ sepBy functionParser (string ",")
      pure (name, functions)
      where
        functionParser :: Parser (Part Int -> String)
        functionParser = do
          f <-
            optional . try $
              liftA2
                (>>>)
                ( choice
                    [ char 'x' $> xValue,
                      char 'm' $> mValue,
                      char 'a' $> aValue,
                      char 's' $> sValue
                    ]
                )
                ( liftA2
                    flip
                    ((char '<' $> (<)) <|> (char '>' $> (>)))
                    (decimal <* char ':')
                )
          s <- some letterChar
          pure $ maybe (const s) (bool "" s .) f

    partParser :: Parser (Part Int)
    partParser = betweenCurly $ do
      x <- string "x=" *> decimal
      m <- string ",m=" *> decimal
      a <- string ",a=" *> decimal
      s <- string ",s=" *> decimal
      pure $ Part x m a s

parseWorkflows :: String -> HashMap String [WorkflowRange]
parseWorkflows =
  fst . breakOn "\n\n"
    >>> lines
    >>> fromList . map (parseInput workflowParser id)
  where
    workflowParser :: Parser (String, [WorkflowRange])
    workflowParser = do
      name <- takeWhileP Nothing (/= '{')
      ranges <- betweenCurly $ sepBy workflowRangeParser (string ",")
      pure (name, ranges)
      where
        workflowRangeParser :: Parser WorkflowRange
        workflowRangeParser = do
          r <-
            optional . try $
              liftA3
                (,,)
                (oneOf "xmas")
                ((char '<' $> LT) <|> (char '>' $> GT))
                (decimal <* char ':')
          s <- some letterChar
          pure $ maybe (WfRange '#' EQ 0 s) (flip (uncurry3 WfRange) s) r

betweenCurly :: Parser a -> Parser a
betweenCurly = between (char '{') (char '}')