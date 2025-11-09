{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -with-rtsopts=-N #-}

module Main (main) where

import ALongWalk (walkLongestDryHike, walkLongestHike)
import Aplenty (acceptedPartRatingCombinationsSum, acceptedPartRatingsSum)
import CamelCards (jokersHandWinningsSum, normalHandWinningsSum)
import ClumsyCrucible (minimumCrucibleHeatLoss, minimumUltraCrucibleHeatLoss)
import Control.Arrow ((>>>))
import CosmicExpansion (hugeExpansionGalaxyPathsSum, shortestGalaxyPathsSum)
import CubeConundrum (fewestCubesPowerSetSum, possibleGamesIdSum)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedDir)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HsMS (fromList, (!))
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GearRatios (gearRatiosSum, partNumbersSum)
import HauntedWasteland (camelEscapeTime, ghostEscapeTime)
import HotSprings (possibleCombinationsSum, unfoldedPossibleCombinationsSum)
import IfYouGiveASeedAFertilizer (nearestSeed, nearestSeedRange)
import LavaductLagoon (lagoonArea, lagoonAreaFixed)
import LensLibrary (initSequenceHashesSum, lensBoxFocusingPowersSum)
import MirageMaintenance (initialValuePredictionsSum, nextValuePredictionsSum)
import NeverTellMeTheOdds (hailstoneCollisionsCount, perfectRockThrowCoordinatesSum)
import ParabolicReflectorDish (platformBeamLoadsSum, spinningPlatformBeamLoadsSum)
import PipeMaze (farthestPipeSteps, nestPipesCount)
import PointOfIncidence (mirrorScoresSum, mirrorSmudgeScoresSum)
import PulsePropagation (cablesWarmUp, machineTurnOnClicks)
import SandSlabs (safeBricksCount, unsafeBrickFallsCount)
import Scratchcards (scratchcardCloneCountsSum, scratchcardPointsSum)
import Snowverload (splitComponentSizesProduct)
import StepCounter (gardenReachablePlotsCount, infiniteGardenReachablePlotsCount)
import System.Environment (getArgs)
import Text.Printf (printf)
import TheFloorWillBeLava (energizedTilesCount, maximumEnergizedTilesCountAllStarts)
import Trebuchet (calibrationValuesSum, fixedCalibrationValuesSum)
import WaitForIt (waysToRecordFullRace, waysToRecordProduct)

---------------------------------------------------------------------------------------------------
-- Resources
resourcesDir :: [(FilePath, ByteString)]
resourcesDir = $(embedDir "src/resources")

getResource :: String -> String
getResource =
  unpack
    . liftA2
      fromMaybe
      (error . ("Resource not found: " ++))
      (`lookup` resourcesDir)

puzzleAnswers :: HashMap String (String, String)
puzzleAnswers =
  getResource "PuzzleAnswers.out"
    & ( lines
          >>> map words
          >>> map listToPuzzleAnswer
          >>> HsMS.fromList
      )
  where
    listToPuzzleAnswer :: [String] -> (String, (String, String))
    listToPuzzleAnswer [p, a1, a2] = (p, (a1, a2))
    listToPuzzleAnswer _ = error "Invalid puzzle answers format"

---------------------------------------------------------------------------------------------------
-- Exports
main :: IO ()
main = do
  args <- getArgs
  let selectedPuzzles =
        if null args
          then replicate 25 True
          else
            [show n `elem` args | n <- [1 :: Int .. 25]]
  putStrLn "AoC 2023 - Haskell\n"
  sequenceA_
    [ prettySolution2 selectedPuzzles (1, "Trebuchet") calibrationValuesSum $ Just fixedCalibrationValuesSum,
      prettySolution2 selectedPuzzles (2, "CubeConundrum") possibleGamesIdSum $ Just fewestCubesPowerSetSum,
      prettySolution2 selectedPuzzles (3, "GearRatios") partNumbersSum $ Just gearRatiosSum,
      prettySolution2 selectedPuzzles (4, "Scratchcards") scratchcardPointsSum $ Just scratchcardCloneCountsSum,
      prettySolution2 selectedPuzzles (5, "IfYouGiveASeedAFertilizer") nearestSeed $ Just nearestSeedRange,
      prettySolution2 selectedPuzzles (6, "WaitForIt") waysToRecordProduct $ Just waysToRecordFullRace,
      prettySolution2 selectedPuzzles (7, "CamelCards") normalHandWinningsSum $ Just jokersHandWinningsSum,
      prettySolution2 selectedPuzzles (8, "HauntedWasteland") camelEscapeTime $ Just ghostEscapeTime,
      prettySolution2 selectedPuzzles (9, "MirageMaintenance") nextValuePredictionsSum $ Just initialValuePredictionsSum,
      prettySolution2 selectedPuzzles (10, "PipeMaze") farthestPipeSteps $ Just nestPipesCount,
      prettySolution2 selectedPuzzles (11, "CosmicExpansion") shortestGalaxyPathsSum $ Just hugeExpansionGalaxyPathsSum,
      prettySolution2 selectedPuzzles (12, "HotSprings") possibleCombinationsSum $ Just unfoldedPossibleCombinationsSum,
      prettySolution2 selectedPuzzles (13, "PointOfIncidence") mirrorScoresSum $ Just mirrorSmudgeScoresSum,
      prettySolution2 selectedPuzzles (14, "ParabolicReflectorDish") platformBeamLoadsSum $ Just spinningPlatformBeamLoadsSum,
      prettySolution2 selectedPuzzles (15, "LensLibrary") initSequenceHashesSum $ Just lensBoxFocusingPowersSum,
      prettySolution2 selectedPuzzles (16, "TheFloorWillBeLava") energizedTilesCount $ Just maximumEnergizedTilesCountAllStarts,
      prettySolution2 selectedPuzzles (17, "ClumsyCrucible") minimumCrucibleHeatLoss $ Just minimumUltraCrucibleHeatLoss,
      prettySolution2 selectedPuzzles (18, "LavaductLagoon") lagoonArea $ Just lagoonAreaFixed,
      prettySolution2 selectedPuzzles (19, "Aplenty") acceptedPartRatingsSum $ Just acceptedPartRatingCombinationsSum,
      prettySolution2 selectedPuzzles (20, "PulsePropagation") cablesWarmUp $ Just machineTurnOnClicks,
      prettySolution2 selectedPuzzles (21, "StepCounter") gardenReachablePlotsCount $ Just infiniteGardenReachablePlotsCount,
      prettySolution2 selectedPuzzles (22, "SandSlabs") safeBricksCount $ Just unsafeBrickFallsCount,
      prettySolution2 selectedPuzzles (23, "ALongWalk") walkLongestHike $ Just walkLongestDryHike,
      prettySolution2 selectedPuzzles (24, "NeverTellMeTheOdds") hailstoneCollisionsCount $ Just perfectRockThrowCoordinatesSum,
      prettySolution2 selectedPuzzles (25, "Snowverload") splitComponentSizesProduct (Nothing :: Maybe (a -> a))
    ]

---------------------------------------------------------------------------------------------------
-- Functions
prettySolution2 :: (Show a, Show b) => [Bool] -> (Int, String) -> (String -> a) -> Maybe (String -> b) -> IO ()
prettySolution2 selectedPuzzles (day, puzzle) solution1 maybeSolution2 =
  if not (selectedPuzzles !! (day - 1))
    then return ()
    else do
      putStrLn (printf "Day %d: %s" day puzzle)
      let input = getResource (puzzle ++ ".in")
      let (answer1, answer2) = puzzleAnswers HsMS.! puzzle
      prettySolution puzzle 1 solution1 input answer1
      case maybeSolution2 of
        Just solution2 -> prettySolution puzzle 2 solution2 input answer2
        Nothing -> putStr ""
      putStrLn ""

prettySolution :: (Show a) => String -> Int -> (String -> a) -> String -> String -> IO ()
prettySolution puzzle part solution input answer = do
  microsecondsStart <- getPOSIXTime
  let output = show $ solution input
  if output == answer
    then putStr $ printf "%d -> %s" part output
    else error $ printf "Wrong solution for %s part %d: expected %s, but got %s" puzzle part (show answer) output
  microsecondsEnd <- getPOSIXTime
  putStrLn $ printf " (%dµs)" (floor ((microsecondsEnd - microsecondsStart) * 1e6) :: Int)