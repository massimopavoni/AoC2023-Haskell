{-# LANGUAGE TupleSections #-}

module MirageMaintenance (nextValuePrediction, initialValuePrediction) where

import CommonUtils (Parser, parseInput)
import Control.Category ((>>>))
import Data.List (foldl1')
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is quite simple, once the predictValue function recursion is understood.
nextValuePrediction :: String -> Int
nextValuePrediction = parseInput valueHistoryParser $ predictValue last sum

-- The second part does not change much compared to the first,
-- as the only change is the function to properly calculate the difference that yields the initial value.
initialValuePrediction :: String -> Int
initialValuePrediction = parseInput valueHistoryParser $ predictValue head (foldl1' (flip (-)))

------------------------------------------------------------------------------------------------
-- Functions

predictValue :: (Eq a, Num a) => ([a] -> a) -> ([a] -> a) -> [a] -> a
predictValue f r =
  ([],)
    >>> until
      (all (== 0) . snd)
      (\(ls, xs) -> (f xs : ls, (zipWith (-) =<< tail) xs))
    >>> r . fst

------------------------------------------------------------------------------------------------
-- Parsers

valueHistoryParser :: Parser [Int]
valueHistoryParser = sepBy1 (signed (pure ()) decimal) (char ' ') <* eof