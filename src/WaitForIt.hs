module WaitForIt (waysToRecord, waysToRecordFullRace) where

import CommonUtils (Parser, parseInput, space)
import Text.Megaparsec (between, eof, sepBy1, some)
import Text.Megaparsec.Char (digitChar, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part maps the bounds size the solutions.
waysToRecord :: String -> [Int]
waysToRecord = parseInput (racesParser False) $ map inequationBoundsSize

-- The second part just gets the bounds size of the one solution, the one race
-- (and racesParser True switches the parser to "one race only mode",
-- also known as "bad kerning mode", lol).
waysToRecordFullRace :: String -> Int
waysToRecordFullRace = parseInput (racesParser True) $ inequationBoundsSize . head

------------------------------------------------------------------------------------------------
-- Functions

-- The problem is very simply solved in a closed-form way by the inequation (t - x) * x > d.
inequationBoundsSize :: (Integral a) => (a, a) -> a
inequationBoundsSize (t, d) =
  1
    + floor (1 / 2 * (t' + discriminant))
    - ceiling (1 / 2 * (t' - discriminant))
  where
    t' :: (Floating a) => a
    t' = fromIntegral t

    d' :: (Floating a) => a
    d' = fromIntegral d

    discriminant :: Double
    discriminant = sqrt (t' ** 2 - 4 * d')

------------------------------------------------------------------------------------------------
-- Parsers

-- I do know that a parser is overkill for many of these problem inputs,
-- but I don't care, it looks nicer, lol.
racesParser :: Bool -> Parser [(Int, Int)]
racesParser sr = do
  tls <- between (string "Time:" <* space) newline numbers
  drs <- between (string "Distance:" <* space) newline numbers <* eof
  pure $ zip tls drs
  where
    numbers :: Parser [Int]
    numbers =
      if sr
        then pure . read . concat <$> sepBy1 (some digitChar) space
        else sepBy1 decimal space
