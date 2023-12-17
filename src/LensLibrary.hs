module LensLibrary where

import Control.Category ((>>>))
import Data.Bool (bool)
import qualified Data.ByteString as BStr (foldl')
import Data.ByteString.Char8 (ByteString, pack, splitWith, unsnoc)
import qualified Data.ByteString.Char8 as BSC8 (init)
import Data.HashMap.Strict (HashMap, empty)
import qualified Data.HashMap.Strict as HsMS (alter, toList)
import qualified Data.List as List (foldl')
import qualified Data.Map.Ordered as MapO (alter)
import Data.Map.Ordered.Strict (OMap, assocs, delete, singleton)

-- This one problem was quite fun to work on, not just for the puzzle itself
-- (because practically all of them are very well thought and fun to read),
-- but because of the different data structures and packages I could use to make it work properly.

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is very easy, as it's just about properly hashing the values.
initSequenceHashes :: String -> [Int]
initSequenceHashes =
  parseInitSequence
    >>> map hash

-- The second part was also quite easy, once the appropriate data structures are used.
lensBoxFocusingPowers :: String -> [Int]
lensBoxFocusingPowers =
  parseInitSequence
    >>> map parseLensOp
    >>> List.foldl' updateBoxes empty
    >>> map (uncurry boxFocusingPower) . HsMS.toList
  where
    -- In particular, we're using a Ordered Map for every box, as we need to keep the lenses sorted by insertion order,
    -- and we fortunately still have a alter function from the normal Ordered Map module,
    -- while getting the other functions from the strict submodule.
    updateBoxes :: HashMap Int (OMap ByteString Int) -> (Int, ByteString, Int) -> HashMap Int (OMap ByteString Int)
    updateBoxes hm (boxH, lbl, lfl) = HsMS.alter executeOp boxH hm
      where
        -- For every lens operation, we have some possibilities:
        -- 1. the box does not exist yet, so we create it with the first lens in it
        -- if the operation is not a removal;
        -- 2. the box does exist, so we just change the Ordered Map,
        -- deleting the lens, adding or updating the lens.
        executeOp :: Maybe (OMap ByteString Int) -> Maybe (OMap ByteString Int)
        executeOp Nothing = if lfl == -1 then Nothing else Just $ singleton (lbl, lfl)
        executeOp (Just lsm) = case lfl of
          -1 -> Just $ delete lbl lsm
          _ -> Just $ MapO.alter (const $ Just lfl) lbl lsm

    boxFocusingPower :: Int -> OMap ByteString Int -> Int
    boxFocusingPower bn = List.foldl' foldLens 0 . zip [1 ..] . assocs
      where
        boxN :: Int
        boxN = bn + 1

        foldLens :: Int -> (Int, (ByteString, Int)) -> Int
        foldLens acc (lp, (_, lfl)) = acc + boxN * lp * lfl

------------------------------------------------------------------------------------------------
-- Functions

-- Within the hash function, I can just use the normal bytestring fold,
-- as it already gets the characters as Word8, and we can turn them into Ints.
hash :: ByteString -> Int
hash = BStr.foldl' (\h c -> ((h + fromIntegral c) * 17) `rem` 256) 0

------------------------------------------------------------------------------------------------
-- Parsers

-- Init is for skipping the newline at the end of the input,
-- as I saved all of my inputs with a final '\n' (just automatically did it by pasting).
parseInitSequence :: String -> [ByteString]
parseInitSequence = splitWith (== ',') . BSC8.init . pack

-- The unsnoc has a very funny name, but it's very useful in this specific instance,
-- for parsing the lens operations depending on the last character,
-- everything else about it is quite self-explanatory
-- (and we're returning a triple so that we already have all
-- the box numbers (label hashes) and the label themselves,
-- as well as the operation (-1 for removing, any digit for adding)).
parseLensOp :: ByteString -> (Int, ByteString, Int)
parseLensOp bs = case unsnoc bs of
  Just (lbl, lfl) ->
    let b = lfl == '-'
        lbl' = bool BSC8.init id b lbl
        lfl' = bool (read [lfl]) (-1) b
     in (hash lbl', lbl', lfl')
  Nothing -> error "Invalid lens operation"