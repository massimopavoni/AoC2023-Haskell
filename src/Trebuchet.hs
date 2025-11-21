module Trebuchet (calibrationValuesSum, fixedCalibrationValuesSum) where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromJust, fromMaybe)
import RandomUtils (Parser, parseInput)
import Text.Megaparsec (anySingle, choice, eof, getInput, lookAhead, manyTill, optional, setInput)
import Text.Megaparsec.Char (digitChar, string)

---------------------------------------------------------------------------------------------------
-- Exports

-- The first part supports a basic solution, and a pretty self-explanatory one at that.
calibrationValuesSum :: String -> Int
calibrationValuesSum =
    lines
        >>> map (read . firstLast . filter isDigit)
        >>> sum
  where
    firstLast :: [a] -> [a]
    firstLast [] = []
    firstLast [x] = [x, x]
    firstLast (x : xs) = [x, last xs]

-- The second part saw my first few attempts fail under the desire to not use many complex external packages.
-- But I ended up using a parsing library anyway, and still learned a lot in the process.
fixedCalibrationValuesSum :: String -> Int
fixedCalibrationValuesSum =
    lines
        >>> map (parseInput valueParser id)
        >>> sum

---------------------------------------------------------------------------------------------------
-- Parsers

-- The value parser ended up using a quirky approach, finding the first digit,
-- reversing the remaining input, and parsing the first (reverse) digit again
-- while accounting for inputs with only one available digit.
valueParser :: Parser Int
valueParser = do
    tens <- manyTill anySingle (lookAhead forwardDigit) >> forwardDigit
    getInput >>= setInput . reverse
    units <- manyTill anySingle (lookAhead $ void reverseDigit <|> eof) >> optional reverseDigit
    pure $ tens * 10 + fromMaybe tens units
  where
    forwardDigit :: Parser Int
    forwardDigit = digit digitWords

    reverseDigit :: Parser Int
    reverseDigit = digit reverseDigitWords

    -- The digit parsers are quite simple, and they mainly rely on the choice between a digit word or character.
    digit :: [String] -> Parser Int
    digit ds = stringToDigit <$> choice (string <$> ds) <|> digitToInt <$> digitChar

    stringToDigit :: String -> Int
    stringToDigit = fromJust . (`lookup` digitsMap)

    digitsMap :: [(String, Int)]
    digitsMap = zip (digitWords ++ reverseDigitWords) $ cycle [1 .. 9]

    digitWords :: [String]
    digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

    reverseDigitWords :: [String]
    reverseDigitWords = reverse <$> digitWords
