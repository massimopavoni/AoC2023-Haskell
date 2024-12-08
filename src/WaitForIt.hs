module WaitForIt (waysToRecordProduct, waysToRecordFullRace) where

import RandomUtils (Parser, parseInput)
import Safe (headErr)
import Text.Megaparsec (between, eof, sepBy1, some)
import Text.Megaparsec.Char (digitChar, hspace1, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part maps the bounds size.
waysToRecordProduct :: String -> Int
waysToRecordProduct = product . parseInput (racesParser False) (map inequationBoundsSize)

-- The second part just gets the bounds size of the one solution, the one race
-- (and racesParser True switches the parser to "one race only mode",
-- also known as "bad kerning mode", lol).
waysToRecordFullRace :: String -> Int
waysToRecordFullRace = parseInput (racesParser True) $ inequationBoundsSize . headErr

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

    discriminant :: Double
    discriminant = sqrt (t' ** 2 - 4 * fromIntegral d)

------------------------------------------------------------------------------------------------
-- Parsers

-- I do know that a parser is overkill for many of these problem inputs,
-- but I don't care, it looks nicer, lol.
racesParser :: Bool -> Parser [(Int, Int)]
racesParser sr = do
  tls <- between (string "Time:" <* hspace1) newline numbers
  drs <- between (string "Distance:" <* hspace1) newline numbers <* eof
  pure $ zip tls drs
  where
    numbers :: Parser [Int]
    numbers =
      if sr
        then pure . read . concat <$> sepBy1 (some digitChar) hspace1
        else sepBy1 decimal hspace1
