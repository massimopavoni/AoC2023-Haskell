module MirageMaintenance (nextValuePrediction, initialValuePrediction) where

import Control.Category ((>>>))
import Data.List (foldl1')
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, sepBy1)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

------------------------------------------------------------------------------------------------
-- Data types

type Parser = Parsec Void String

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is quite simple, once the predictValue function recursion is understood.
nextValuePrediction :: String -> Int
nextValuePrediction =
  either (error . errorBundlePretty) id . parse valueHistoryParser ""
    >>> predictValue last sum

-- The second part does not change much compared to the first,
-- as the only change is the function to properly calculate the difference that yields the initial value.
initialValuePrediction :: String -> Int
initialValuePrediction =
  either (error . errorBundlePretty) id . parse valueHistoryParser ""
    >>> predictValue head (foldl1' (flip (-)))

------------------------------------------------------------------------------------------------
-- Functions

predictValue :: (Eq a, Num a) => ([a] -> a) -> ([a] -> a) -> [a] -> a
predictValue f r = go []
  where
    go ls xs
      | all (== 0) xs = r ls
      | otherwise = go (f xs : ls) (differences xs)
      where
        differences :: (Num a) => [a] -> [a]
        differences = zipWith (-) =<< tail

------------------------------------------------------------------------------------------------
-- Parsers

valueHistoryParser :: Parser [Int]
valueHistoryParser = sepBy1 (signed (pure ()) decimal) (char ' ') <* eof