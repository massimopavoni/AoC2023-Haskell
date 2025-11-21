{-# LANGUAGE TupleSections #-}

module MirageMaintenance (nextValuePredictionsSum, initialValuePredictionsSum) where

import Control.Category ((>>>))
import Data.List (foldl1')
import RandomUtils (Parser, parseInput)
import Safe (headErr, tailSafe)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

---------------------------------------------------------------------------------------------------
-- Exports

-- The first part is quite simple, once the predictValue function recursion is understood.
nextValuePredictionsSum :: String -> Int
nextValuePredictionsSum =
    lines
        >>> map (parseInput valueHistoryParser $ predictValue last sum)
        >>> sum

-- The second part does not change much compared to the first,
-- as the only change is the function to properly calculate the difference that yields the initial value.
initialValuePredictionsSum :: String -> Int
initialValuePredictionsSum =
    lines
        >>> map (parseInput valueHistoryParser $ predictValue headErr (foldl1' (flip (-))))
        >>> sum

---------------------------------------------------------------------------------------------------
-- Functions

predictValue :: (Eq a, Num a) => ([a] -> a) -> ([a] -> a) -> [a] -> a
predictValue f r =
    ([],)
        >>> until
            (all (== 0) . snd)
            (\(ls, xs) -> (f xs : ls, (zipWith (-) =<< tailSafe) xs))
        >>> r . fst

---------------------------------------------------------------------------------------------------
-- Parsers

valueHistoryParser :: Parser [Int]
valueHistoryParser = sepBy1 (signed (pure ()) decimal) (char ' ') <* eof
