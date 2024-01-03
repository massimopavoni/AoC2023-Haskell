module PulsePropagation (cablesWarmUp, machineTurnOnClicks) where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Category ((>>>))
import Data.Foldable (find, foldl')
import Data.Function ((&))
import Data.Functor (($>))
import Data.Map.Strict (Map, adjust, elems, empty, fromList, insert, keys, member, notMember, (!))
import Data.List (foldl1', iterate')
import Data.Maybe (fromJust)
import Data.Tuple.Extra (both)
import RandomUtils (Parser, parseInput)
import Text.Megaparsec (between, eof, sepBy1, some)
import Text.Megaparsec.Char (char, letterChar, string)

-- It seems a bit controversial that to solve the second part of this puzzle,
-- a more thorough analysis of the actual input is needed:
-- realizing that the modules network is split into four different "subnets" that cycle
-- and align on a conjuction by design is a bit meh, but reminiscent of "Haunted Wasteland",
-- which I believe was the point here (remembering that cycles and LCM come together
-- to greatly reduce the computation time, especially with very few common factors and all big primes).

------------------------------------------------------------------------------------------------
-- Data types

data Pulse = Low | High
  deriving (Eq, Show)

-- A module can be a broadcaster, a flip-flop or a conjuction:
-- the first one only has output connections, the second one also has a switch state
-- (we represent off as Low and on as High), and the third one has memory instead
-- (the last pulses received by the conjuction's inputs).
data Module
  = Broadcaster {outputs :: [String]}
  | FlipFlop {switch :: Pulse, outputs :: [String]}
  | Conjuction {memory :: Map String Pulse, outputs :: [String]}
  deriving (Show)

-- A configuration of the module network identifies a specific state of the network's module fields,
-- the number of button clicks performed, the number of pulses that travelled through the cables so far,
-- and the potential cycles found on the "rx" father conjuction module inputs.
data Configuration = Config
  { clicks :: Int,
    pulses :: (Int, Int),
    cycles :: Map String Int,
    modules :: Map String Module
  }
  deriving (Show)

------------------------------------------------------------------------------------------------
-- Exports

-- The first part only needs to click button 1000 times and count the number of pulses in the entire network.
cablesWarmUp :: String -> Int
cablesWarmUp =
  parseModules
    >>> Config 0 (0, 0) empty
    >>> iterate' singleButtonClick
    >>> take 1000 . tail
    >>> unzip . map pulses
    >>> both sum
    >>> uncurry (*)

-- The second part instead has to keep track of the status of certain conjunction modules which connect
-- to a final module and that one connects to the "rx" module: by keeping track of the pulses sent
-- by these specific last (4) modules, we can get some cycles duration,
-- and once we have them all we can get the actual number of clicks needed to turn on the machine
-- by computing the least common multiple of the cycle durations found.
machineTurnOnClicks :: String -> Int
machineTurnOnClicks =
  parseModules
    >>> finishedCyclesConfiguration
    >>> foldl1' lcm . elems . cycles
  where
    finishedCyclesConfiguration :: Map String Module -> Configuration
    finishedCyclesConfiguration ms =
      iterate' singleButtonClick (Config 0 (0, 0) empty ms)
        & fromJust . find rxInputsFound . tail
      where
        rxInputsFound :: Configuration -> Bool
        rxInputsFound (Config _ _ cs _) = all (`elem` keys cs) rxInputs

        rxInputs :: [String]
        rxInputs =
          elems ms
            & ( fromJust . find (elem "rx" . outputs)
                  >>> keys . memory
              )

------------------------------------------------------------------------------------------------
-- Functions

-- The single button click function is quite convoluted.
-- We start with the basic configuration, and we update it starting from the "broadcaster" module.
singleButtonClick :: Configuration -> Configuration
singleButtonClick (Config cks _ cs ms) = updateConfig [("", "broadcaster", Low)] (Config (cks + 1) (1, 0) cs ms)
  where
    -- During the update, we have some cases to end the pulse propagation
    -- (the recursion), and some to continue it:
    -- 1. the list of pulses to propagate is empty, meaning the button can be clicked again;
    -- 2. the current module is the broadcaster, so we recurse with the list of modules
    -- to which the broadcaster...broadcasted, while just increasing the number of low pulses sent;
    -- 3. now we have to distinguish between more cases:
    -- -- 3.1 the current module is not in the configuration (meaning it only appears as an output,
    -- -- and with this specific problem tailored input, meaning it's always the "rx" module),
    -- -- so we continue with the remaining pulses, not changing the configuration;
    -- -- 3.2 a high pulse with the current module being a flip-flop means we ignore skip over
    -- -- the next pulse to propagate, but we still potentially update
    -- -- the configuration with new cycles found;
    -- -- 3.3 in all the other cases we add new pulses to propagate to the end of the list,
    -- -- while also updating the configuration pulses count, cycles found and modules' states.
    updateConfig :: [(String, String, Pulse)] -> Configuration -> Configuration
    updateConfig [] c = c
    updateConfig [(_, "broadcaster", Low)] c =
      let bos = outputs $ modules c ! "broadcaster"
       in updateConfig [("broadcaster", o, Low) | o <- bos] c {pulses = first (+ length bos) (pulses c)}
    updateConfig ((sm, mn, p) : mps) c
      | notMember mn $ modules c = updateConfig mps c
      | p == High && not (isConjuctionModule m) = updateConfig mps c {cycles = cycles'}
      | otherwise =
          updateConfig
            (mps ++ [(mn, o, p') | o <- outputs m'])
            c {pulses = updatedPulses, cycles = cycles', modules = adjust (const m') mn $ modules c}
      where
        m :: Module
        m = modules c ! mn

        p' :: Pulse
        p' = nextPulse m'

        m' :: Module
        m'
          | isConjuctionModule m = m {memory = adjust (const p) sm $ memory m}
          | switch m == Low = m {switch = High}
          | otherwise = m {switch = Low}

        nextPulse :: Module -> Pulse
        nextPulse mm
          | isConjuctionModule mm =
              if all (== High) . elems $ memory mm
                then Low
                else High
          | otherwise = switch mm

        updatedPulses :: (Int, Int)
        updatedPulses =
          (if p' == Low then first else second)
            (+ length (outputs m'))
            $ pulses c

        cycles' :: Map String Int
        cycles'
          | p /= High || "rx" `notElem` outputs m = cycles c
          | otherwise = insert sm (clicks c) $ cycles c

isConjuctionModule :: Module -> Bool
isConjuctionModule Conjuction {} = True
isConjuctionModule _ = False

------------------------------------------------------------------------------------------------
-- Parsers

parseModules :: String -> Map String Module
parseModules =
  lines
    >>> fromList . map (parseInput moduleParser id)
    >>> foldl' initMemories <*> keys
  where
    moduleParser :: Parser (String, Module)
    moduleParser = do
      (m, n) <-
        string "broadcaster" $> (Broadcaster, "broadcaster")
          <|> liftA2
            (,)
            ((char '%' $> FlipFlop Low) <|> (char '&' $> Conjuction empty))
            idParser
      os <- between (string " -> ") eof $ sepBy1 idParser (string ", ")
      pure (n, m os)
      where
        idParser :: Parser String
        idParser = some letterChar

    -- After parsing the modules' info, we have to initialize the conjuction modules' memory,
    -- so that when starting the pulse propagation we have the initial configuration of the puzzle description.
    -- (We have to do this after parsing all the modules because the memory field depends
    -- on information that's not available directly from the input, as opposed to the outputs field,
    -- or the switch field of flip-flop modules, which can both be initialized right away.)
    initMemories :: Map String Module -> String -> Map String Module
    initMemories hm n =
      foldl'
        (flip $ adjust updateMemory)
        hm
        [out | out <- outputs (hm ! n), out `member` hm, isConjuctionModule (hm ! out)]
      where
        updateMemory :: Module -> Module
        updateMemory m = m {memory = insert n Low $ memory m}