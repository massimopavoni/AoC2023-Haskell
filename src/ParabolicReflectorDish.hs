{-# LANGUAGE OverloadedStrings #-}

module ParabolicReflectorDish (platformBeamLoadsSum, spinningPlatformBeamLoadsSum) where

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Data.ByteString.Char8 (ByteString, count, elemIndices, intercalate, pack, sort, splitWith, transpose)
import qualified Data.ByteString.Char8 as BSC8 (length, reverse)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap, empty, insert, (!?))
import RandomUtils (Pos)

---------------------------------------------------------------------------------------------------
-- Exports

-- The first part was very straightforward, and I was able to come up with a solution that
-- does not even manipulate the original input, but just counts the number of round rocks,
-- then uses that information to find the indices to use for the loads summations.
platformBeamLoadsSum :: String -> Int
platformBeamLoadsSum =
  map pack . lines
    >>> northBeamLoads
    >>> sum
  where
    northBeamLoads :: [ByteString] -> [Int]
    northBeamLoads ls =
      let beamLength = length ls
       in transpose ls
            & map
              ( splitWith (== '#')
                  >>> map (BSC8.length &&& count 'O')
                  >>> foldl' addLoad (beamLength, 0)
                  >>> snd
              )
      where
        addLoad :: Pos -> Pos -> Pos
        addLoad (rl, tl) (cl, oc) = (rl - cl - 1, tl + sum [rl - oc + 1 .. rl])

-- The second part was annoyingly incredibly slow:
-- I get that it's not about developing the most efficient solution,
-- but I still went out of my way to try and optimize some parts of it,
-- managing to practically quarter the execution time (making use of ByteString and a HashMap).
-- Other than that, the solution is similar to some previous ones,
-- in the sense that we have to use a different approach for a variation of the puzzle.
-- In this case, we had to find a repeating platform pattern after a certain number of tilting cycles:
-- once figured how to get that, we can just use modular arithmetic to find the platform pattern
-- after the given number of cycles.
spinningPlatformBeamLoadsSum :: String -> Int
spinningPlatformBeamLoadsSum =
  map pack . lines
    >>> iterate spinOnce
    >>> liftA2
      (!!)
      id
      ((\(b, e) -> b + (1000000000 - b) `rem` (e - b)) . spinCycle empty 0)
    >>> transpose
    >>> map calculateLoad
    >>> sum
  where
    -- Once we find an already seen platform pattern, we can stop spinnning
    -- and just yield the indices of the first cycle.
    spinCycle :: HashMap [ByteString] Int -> Int -> [[ByteString]] -> Pos
    spinCycle _ _ [] = error "No spinning cycles"
    spinCycle m i (p : ps) = case m !? p of
      Nothing -> spinCycle (insert p i m) (i + 1) ps
      Just s -> (s, i)

    -- The tilting function is important but also the villain of all of it,
    -- as it's the function that takes the most to execute,
    -- mainly because of the double reverse and the many sorting operations on substrings.
    -- The spin dataflow essentially represents the four cardinal directions:
    -- for north and south, we need to operate on columns and so we have to double transpose,
    -- while for north and west we also need to reverse the strings to sort in the correct order.
    -- Now, that last statement might seem wrong, so here's why we should reverse for those directions:
    -- the correct order should be round rocks first, then empty spaces, and finally cube rocks,
    -- but it turns out we can just use the natural order of the ASCII characters,
    -- which is # < . < O, meaning that left to right is actually reverse that.
    -- (Without this little trick, we could've had to either use a custom data type,
    -- which would've been too much for the problem and could've made the solution even slower
    -- because of not being able to use ByteStrings (and Vectors or other similar types
    -- would've needed a custom transpose implementation (maybe just Matrix could've been used)),
    -- or more reasonably we could've just used a different comparing function,
    -- but then I don't think ByteStrings would've been still available, since the package
    -- does not provide a sortOn function.)
    spinOnce :: [ByteString] -> [ByteString]
    spinOnce =
      transpose . tilt BSC8.reverse . transpose
        >>> tilt BSC8.reverse
        >>> transpose . tilt id . transpose
        >>> tilt id
      where
        tilt :: (ByteString -> ByteString) -> [ByteString] -> [ByteString]
        tilt bsTransform = map (bsTransform . intercalate "#" . map sort . splitWith (== '#') . bsTransform)

    -- Now, this, this is so stupid! I lost an hour or more trying to figure out why
    -- my part two was not working, and it turns out I was doing the same thing I did for the first part,
    -- therefore adding a final (fake, as I described in part one) tilt towards north,
    -- while the problem description wasn't very clear about just spinning and then just summing
    -- the north beam loads at the end (which is easier, because no other tilt is needed,
    -- as they were already part of the earlier spinning), but that doesn't make any sense to me,
    -- for how the problem is described. I guess it could just be me being tired and not really
    -- understanding the funny story context of the reflector dish weird rock device.
    calculateLoad :: ByteString -> Int
    calculateLoad rs =
      let l = BSC8.length rs
       in sum [l - i | i <- elemIndices 'O' rs]
