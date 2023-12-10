module CommonUtils where

import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, some)
import Text.Megaparsec.Char (char)

------------------------------------------------------------------------------------------------
-- Data types

type Parser = Parsec Void String

------------------------------------------------------------------------------------------------
-- Functions

parseInput :: Parser a -> (a -> b) -> String -> b
parseInput p f = either (error . errorBundlePretty) f . parse p ""

------------------------------------------------------------------------------------------------
-- Parsers

space :: Parser String
space = some $ char ' '
