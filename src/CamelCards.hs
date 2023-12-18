module CamelCards (handWinningsNormal, handWinningsJokers) where

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Data.Function (on)
import Data.List (group, sortBy)
import Data.Maybe (fromJust)
import RandomUtils (Parser, parseInput)
import Text.Megaparsec (count, eof, notFollowedBy, oneOf, sepBy1, try)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

------------------------------------------------------------------------------------------------
-- Data types

data Hand = Hand {cards :: String, bid :: Int}

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Eq, Ord)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part is simple because it's just about using the hand winnings function with
-- a tuple reduction sort of identity and the normal order of cards.
handWinningsNormal :: String -> [Int]
handWinningsNormal = handWinnings (\(ht, h, _) -> (ht, h)) "23456789TJQKA"

-- The second part had me call the elf a m*****f***** for the rest of the puzzle, ahah,
-- because the joker change was a funny one; and so we just gotta use a different cards order
-- and a hand type transformation that depends on the first card group (it's always the jokers, if present).
handWinningsJokers :: String -> [Int]
handWinningsJokers = handWinnings handTypeTransform "J23456789TQKA"
  where
    handTypeTransform :: (HandType, Hand, [String]) -> (HandType, Hand)
    handTypeTransform (ht, h, csg) = if head firstCardsGroup == 'J' then (improveHandType, h) else (ht, h)
      where
        firstCardsGroup :: String
        firstCardsGroup = head csg

        -- A quite simple function, once I laid out a table of the possible options:
        -- 1. a high card can have 1 joker at most (which is always not the actual high card),
        -- and so it always becomes a one pair;
        -- 2. a pair can have 2 jokers at most, and the pair can either be a normal one and have a joker elsewhere
        -- or be a pair of jokers, and in both cases it always becomes a three of a kind;
        -- 3. a two pair can have 2 jokers at most, and the pairs can either be two normal pairs,
        -- in which case the one joker is adding to one of them to make a full house, or there could be a pair of jokers,
        -- making the two pair a four of a kind;
        -- 4. a three of a kind can 1 or 3 jokers, but not 2, and in both cases it becomes a four of a kind;
        -- 5. a full house can have 2 or 3 jokers, but not 1, and both cases make it a five of a kind;
        -- 6. a four of a kind can only have 1 or 4 jokers, and both cases result in a five of kind;
        -- 7. a five of a kind is always a five of a kind.
        improveHandType :: HandType
        improveHandType = case ht of
          HighCard -> OnePair
          OnePair -> ThreeOfAKind
          TwoPair -> if length firstCardsGroup == 1 then FullHouse else FourOfAKind
          ThreeOfAKind -> FourOfAKind
          FullHouse -> FiveOfAKind
          FourOfAKind -> FiveOfAKind
          FiveOfAKind -> FiveOfAKind

------------------------------------------------------------------------------------------------
-- Functions

-- The handWinnings function takes the cards order and a function that potentially changes the hand type,
-- then the rest of the dataflow is pretty self-explanatory, maybe except the first composition:
-- first we create a tuple with the cards group, then we use that to determine the hand type,
-- then we apply the transformation (which is sort of an id if we're not playing with jokers).
handWinnings :: ((HandType, Hand, [String]) -> (HandType, Hand)) -> String -> String -> [Int]
handWinnings httf cso =
  parseInput handsParser $
    map (httf . addHandType . addCardsGroups)
      >>> map snd . sortBy compareRanks
      >>> zipWith (curry (liftA2 (*) fst (bid . snd))) [1 ..]
  where
    addCardsGroups :: Hand -> (Hand, [String])
    addCardsGroups = id &&& group . sortBy compareCards . cards

    -- Detecting the hand type is another important thing, and it's done by analyzing the
    -- number of groups of equal cards:
    -- 1. 5 means they're all different, so it's a high card;
    -- 2. 4 means there's a group of 2, so it's a one pair;
    -- 3. 3 could mean there's two groups of 2 and one of 1, meaning it's a two pair,
    -- or there's a group of 3 and two of 1, meaning it's a three of a kind;
    -- 4. 2 could mean there's a group of 3 and one of 2, making a full house,
    -- or there's a group of 4 and one of 1, making a four of a kind;
    -- 5. 1 means they're all the same, so it's a five of a kind.
    addHandType :: (Hand, [String]) -> (HandType, Hand, [String])
    addHandType (h, csg) =
      let csgl = length <$> csg
       in ( case length csg of
              5 -> HighCard
              4 -> OnePair
              3 -> if 2 `elem` csgl then TwoPair else ThreeOfAKind
              2 -> if 3 `elem` csgl then FullHouse else FourOfAKind
              1 -> FiveOfAKind
              _ -> error "Invalid hand",
            h,
            csg
          )

    compareRanks :: (HandType, Hand) -> (HandType, Hand) -> Ordering
    compareRanks (ht1, h1) (ht2, h2) = case compare ht1 ht2 of
      EQ -> compareHands h1 h2
      x -> x
      where
        -- Looking at mconcat had me trying to understand why we use a Monoid operation,
        -- and it reminded me of the algebra concepts I studied in university a few years ago,
        -- and I'm realizing how gorgeous the type system and the Haskell algebraic structures hierarchy are.
        -- These little things are starting to make me fall into the Haskell rabbit hole deeper and deeper.
        -- (Also, I found myself getting rid of the initial Ord instance I had defined for Hand,
        -- exactly because of the change in the cards order introduced by the second part,
        -- and because I didn't want to have a different Hand data types for each part of the puzzle.)
        compareHands :: Hand -> Hand -> Ordering
        compareHands (Hand c1 _) (Hand c2 _) = mconcat $ zipWith compareCards c1 c2

    compareCards :: Char -> Char -> Ordering
    compareCards = compare `on` cardValue
      where
        cardValue :: Char -> Int
        cardValue = fromJust . (`lookup` zip cso [2 .. 14])

------------------------------------------------------------------------------------------------
-- Parsers

handsParser :: Parser [Hand]
handsParser =
  sepBy1
    (liftA2 Hand (count 5 $ oneOf "23456789TJQKA") (char ' ' *> decimal))
    (try $ newline <* notFollowedBy eof)
    <* newline
    <* eof
