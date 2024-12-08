module NeverTellMeTheOdds (hailstoneCollisionsCount, perfectRockThrowCoordinatesSum) where

import Control.Arrow (second, (&&&))
import Control.Category ((>>>))
import Control.Monad (join)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (both)
import Numeric.LinearAlgebra (Vector, cross, det, dot, find, fromColumns, scale, toList, vector)
import RandomUtils (Parser, parseInput)
import Text.Megaparsec (sepBy1, try)
import Text.Megaparsec.Char (char, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

------------------------------------------------------------------------------------------------
-- Data types

-- We use list of doubles for hailstone position and velocity
-- so that we can easily convert them to vectors later.
data Hailstone = Hailstone
  { position :: [Double],
    velocity :: [Double]
  }
  deriving (Show)

type VecTuple = (Vector Double, Vector Double)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part can be solved by finding all the collisions
-- happening in the future of the hailstones' paths for every pair.
-- Since the number of hailstones is not too high, this is just about using
-- line-line intersection equations to find the time of collision and verify
-- that it happens within 0 and 1 nano-second from the starting position.
hailstoneCollisionsCount :: String -> Int
hailstoneCollisionsCount =
  parseHailstones
    >>> map hailstoneToLine2D
    >>> join (zipIf (<))
    >>> mapMaybe (uncurry linesIntersection)
    >>> length
  where
    -- This part only deals in 2 dimensions.
    hailstoneToLine2D :: Hailstone -> VecTuple
    hailstoneToLine2D =
      position &&& velocity
        >>> both (vector . take 2)
        >>> fst &&& uncurry (+)

    zipIf :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
    zipIf condition xs ys = [(x, y) | x <- xs, y <- ys, condition x y]

    -- Intersections are found simply by following the closed-form equations;
    -- the one nice touch is checking the determinant values for invalid solutions
    -- before actually calculating the intersection time and point.
    linesIntersection :: VecTuple -> VecTuple -> Maybe (Vector Double)
    linesIntersection (p1, p2) (p3, p4)
      | den == 0
          || signum tNum /= signum den
          || signum uNum == signum den =
          Nothing
      | null $
          find
            ( liftA2
                (||)
                (< 200000000000000)
                (400000000000000 <)
            )
            p =
          Just p
      | otherwise = Nothing
      where
        p :: Vector Double
        p = let t = tNum / den in p1 + scale t (p2 - p1)

        tNum :: Double
        tNum = det (fromColumns [p1 - p3, p3 - p4])

        uNum :: Double
        uNum = det (fromColumns [p1 - p2, p1 - p3])

        den :: Double
        den = det (fromColumns [p1 - p2, p3 - p4])

-- The second part presents a more difficult challenge, one I didn't know could be solved
-- still in closed-form, actually even faster than the first part.
-- Since we are looking for a trajectory in 3 dimensions that passes through all 300 hailstones' future paths,
-- a naive approach would translate this into a 306-dimensional linear programming problem with 1200 constraints.
-- Such a problem is easily solvable by any solver that allows arithmetic constraints (SMT based ones),
-- and indeed I initially thought of using something like the sbv package
-- (although I would've been happier to find a good pure Haskell library, and if I did I would've probably missed the better solution).
-- The best idea is instead to choose 3 hailstones (any three, but we just pick the first ones) and
-- shift the frame of reference using the first one: the cross product condition of colinearity
-- for the 2 remaining hailstones can be used to find the collision times.
-- Some more simple calculations lead to the initial position and velocity of the perfect rock throw.
perfectRockThrowCoordinatesSum :: String -> Int
perfectRockThrowCoordinatesSum =
  parseHailstones
    >>> take 3
    >>> map hailstoneToPosVel3D
    >>> findPerfectThrow
    >>> both toList
    >>> round . sum . fst
  where
    -- And this part deals in 3 dimensions (plus the time).
    hailstoneToPosVel3D :: Hailstone -> VecTuple
    hailstoneToPosVel3D = both vector . (position &&& velocity)

    findPerfectThrow :: [VecTuple] -> VecTuple
    findPerfectThrow [h0, h1, h2] =
      let -- Shifted frame of reference position and velocity vectors for the 2nd and 3rd hailstones
          (p1, p2) = both (subtract (fst h0) . fst) (h1, h2)
          (v1, v2) = both (subtract (snd h0) . snd) (h1, h2)
          -- Useful cross products for calculating collision times.
          p1p2 = cross p1 p2
          (v1p2, p1v2) = both (uncurry cross) ((v1, p2), (p1, v2))
          -- Collision times are found after cancelling the t1*t2 term in the the colinearity equation
          -- (along one of the other ones) by multiplying by v2 and v1 respectively.
          (t1, t2) =
            both
              (negate . uncurry (/) . ((dot p1p2 . snd) &&& uncurry dot))
              ((v1p2, v2), (p1v2, v1))
          -- The collision positions use the original frame of reference;
          -- we can then use them to find the perfect rock throw velocity and starting position.
          (c1, c2) =
            both
              (uncurry (+) . ((fst . snd) &&& (uncurry scale . second snd)))
              ((t1, h1), (t2, h2))
          rv = scale (1 / (t2 - t1)) (c2 - c1)
       in (c1 - scale t1 rv, rv)
    findPerfectThrow _ = error "Expected exactly 3 hailstones"

------------------------------------------------------------------------------------------------
-- Parsers

-- A very simple parser is used for hailstone information;
-- could've probably just been string manipulation instead,
-- but after so much time not working on the AoC
-- I wanted to try and recall a bit of what I had learned about Megaparsec.
parseHailstones :: String -> [Hailstone]
parseHailstones =
  lines
    >>> map (parseInput hailstoneParser id)
  where
    hailstoneParser :: Parser Hailstone
    hailstoneParser = Hailstone <$> vecParser <* string " @ " <*> vecParser
      where
        vecParser :: Parser [Double]
        vecParser =
          map (fromIntegral :: Int -> Double)
            <$> sepBy1
              (signed (pure ()) decimal)
              (try $ char ',' <* space)