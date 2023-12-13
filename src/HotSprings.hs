{-# LANGUAGE OverloadedStrings #-}

module HotSprings (possibleCombinations, possibleCombinationsUnfolded) where

import Control.Arrow ((***))
import Control.Category ((>>>))
import Data.Array (Array, array, (!))
import Data.ByteString.Char8 (ByteString, cons, index, intercalate, pack)
import qualified Data.ByteString.Char8 as BSC8 (drop, length, notElem, take, uncons)
import Data.List.Split (splitOn)
import Data.Vector (Vector, fromList)
import qualified Data.Vector as Vect (concat, drop, length, uncons)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is easy enough, once the dynamic programming problem is understood.
possibleCombinations :: String -> Int
possibleCombinations = uncurry validArrangementsMemoized . parseRecord

-- The second part is instead quite much slower, and needing at least some memoization.
possibleCombinationsUnfolded :: Int -> String -> Int
possibleCombinationsUnfolded uf = uncurry validArrangementsMemoized . unfold . parseRecord
  where
    unfold :: (ByteString, Vector Int) -> (ByteString, Vector Int)
    unfold (drs, udrs) = (intercalate "?" $ replicate uf drs, Vect.concat $ replicate uf udrs)

------------------------------------------------------------------------------------------------
-- Functions

-- And the memoization here comes, but it's still not enough to make up for a quick solution
-- (to be executed in ghci, because once compiled the slowness it's really not so evident,
-- and that's the reason why I don't care that it runs in 0.7s instead of something like 0.1s).
-- Still, I wasn't able to come up with all of this completely on my own, but I'm still proud
-- of understanding how the solution works, and of how I slightly improved some bits of it.
validArrangementsMemoized :: ByteString -> Vector Int -> Int
validArrangementsMemoized damagedRecord undamagedRecord = validArrangementsMemoized' damagedRecord undamagedRecord
  where
    drsl :: Int
    drsl = BSC8.length damagedRecord

    udrsl :: Int
    udrsl = Vect.length undamagedRecord

    -- This is the actual call to the memoized table with the subproblems' results.
    validArrangementsMemoized' :: ByteString -> Vector Int -> Int
    validArrangementsMemoized' dr udr = validArrangementsTable ! (drsl - BSC8.length dr, udrsl - Vect.length udr)

    -- And this is the memoized table itself, making use of Haskell's laziness to avoid loading the entire array.
    validArrangementsTable :: Array (Int, Int) Int
    validArrangementsTable =
      array
        ((0, 0), (drsl, udrsl))
        [ ((d, u), validArrangement (BSC8.drop d damagedRecord) (Vect.drop u undamagedRecord))
          | d <- [0 .. drsl],
            u <- [0 .. udrsl]
        ]
      where
        -- The function that finds the number of valid arrangements itself has 6 cases:
        -- 1. damaged record and undamaged record lists are both empty, which means that the analyzed arrangement is valid;
        -- 2. damaged record list is empty, but the undamaged one is not,
        -- meaning the arrangement cannot correspond to the considered actual record;
        -- 3. damaged record starts with a damaged hot spring, but undamaged record list is empty,
        -- meaning the arrangement has more hot springs than the undamaged record says;
        -- 4. damaged record starts with an operational hot spring, meaning we can skip that
        -- and check if we already have a valid arrangement count for the rest of the record;
        -- 5. damaged record starts with a damaged hot spring, but the undamaged record list is not empty,
        -- which means that we need to check if we should continue analyzing this arrangement:
        -- -- 5.1 the length of the damaged record should be at least the length of the next damaged group,
        -- -- while that amount of next characters should not be any operational hot springs,
        -- -- while we either have that the entire damaged record is the entire next damaged group,
        -- -- or we don't have another damaged hot spring after this group (as that would mean that
        -- -- the damaged record has more hot springs than the undamaged record says);
        -- -- 5.2 after checking all these conditions, we can skip the damaged group and continue analyzing
        -- -- the arrangement through the memoization calls, and if the checks failed, we just know that
        -- -- the arrangement is not valid;
        -- 6. damaged record starts with an unknown hot spring, meaning we need to count the amount of
        -- valid arrangements in both possible branches (the unknown could be an operational or a damaged hot spring).
        -- I know I probably didn't explain it very well, but I swear I know what's happening.
        validArrangement :: ByteString -> Vector Int -> Int
        validArrangement bs vi = case (BSC8.uncons bs, Vect.uncons vi) of
          (Nothing, Nothing) -> 1
          (Nothing, _) -> 0
          (Just ('#', _), Nothing) -> 0
          (Just ('.', drs), _) -> validArrangementsMemoized' drs vi
          (Just ('#', _), Just (udr, udrs)) ->
            let ldrs = BSC8.length bs
             in if ldrs >= udr
                  && '.' `BSC8.notElem` BSC8.take udr bs
                  && (ldrs == udr || bs `index` udr /= '#')
                  then validArrangementsMemoized' (BSC8.drop (udr + 1) bs) udrs
                  else 0
          (Just ('?', drs), _) -> validArrangementsMemoized' drs vi + validArrangement (cons '#' drs) vi
          _ -> error "Invalid input"

------------------------------------------------------------------------------------------------
-- Parsers

parseRecord :: String -> (ByteString, Vector Int)
parseRecord =
  words
    >>> list2Tuple
    >>> pack *** (fromList . map read . splitOn ",")
  where
    list2Tuple :: [a] -> (a, a)
    list2Tuple [dr, udr] = (dr, udr)
    list2Tuple _ = error "Invalid bigger list"