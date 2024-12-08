{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -with-rtsopts=-N #-}

module Main (main) where

import ALongWalk (walkLongestDryHike, walkLongestHike)
import Aplenty (acceptedPartRatingCombinationsSum, acceptedPartRatingsSum)
import CamelCards (jokersHandWinningsSum, normalHandWinningsSum)
import ClumsyCrucible (minimumCrucibleHeatLoss, minimumUltraCrucibleHeatLoss)
import Control.Applicative (liftA3)
import Control.Arrow ((>>>))
import CosmicExpansion (hugeExpansionGalaxyPathsSum, shortestGalaxyPathsSum)
import CubeConundrum (fewestCubesPowerSetSum, possibleGamesIdSum)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedDir)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HsMS (fromList, (!))
import Data.Maybe (fromMaybe)
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
import Text.Printf (printf)
import TheFloorWillBeLava (energizedTilesCount, maximumEnergizedTilesCountAllStarts)
import Trebuchet (calibrationValuesSum, fixedCalibrationValuesSum)
import WaitForIt (waysToRecordFullRace, waysToRecordProduct)

------------------------------------------------------------------------------------------------
-- Resources
resourcesDir :: [(FilePath, ByteString)]
resourcesDir = $(embedDir "src/resources")

resourceToString :: String -> String
resourceToString =
  unpack
    . liftA2
      fromMaybe
      (error . ("Resource not found: " ++))
      (`lookup` resourcesDir)

puzzleAnswers :: HashMap String (String, String)
puzzleAnswers =
  resourceToString "PuzzleAnswers.out"
    & ( lines
          >>> map words
          >>> map listToPuzzleAnswer
          >>> HsMS.fromList
      )
  where
    listToPuzzleAnswer :: [String] -> (String, (String, String))
    listToPuzzleAnswer [p, a1, a2] = (p, (a1, a2))
    listToPuzzleAnswer _ = error "Invalid puzzle answers format"

------------------------------------------------------------------------------------------------
-- Exports
main :: IO ()
main = do
  putStrLn "AoC 2023 - Haskell\n"
  sequenceA_
    [ prettySolution2 (1, "Trebuchet") calibrationValuesSum $ Just fixedCalibrationValuesSum,
      prettySolution2 (2, "CubeConundrum") possibleGamesIdSum $ Just fewestCubesPowerSetSum,
      prettySolution2 (3, "GearRatios") partNumbersSum $ Just gearRatiosSum,
      prettySolution2 (4, "Scratchcards") scratchcardPointsSum $ Just scratchcardCloneCountsSum,
      prettySolution2 (5, "IfYouGiveASeedAFertilizer") nearestSeed $ Just nearestSeedRange,
      prettySolution2 (6, "WaitForIt") waysToRecordProduct $ Just waysToRecordFullRace,
      prettySolution2 (7, "CamelCards") normalHandWinningsSum $ Just jokersHandWinningsSum,
      prettySolution2 (8, "HauntedWasteland") camelEscapeTime $ Just ghostEscapeTime,
      prettySolution2 (9, "MirageMaintenance") nextValuePredictionsSum $ Just initialValuePredictionsSum,
      prettySolution2 (10, "PipeMaze") farthestPipeSteps $ Just nestPipesCount,
      prettySolution2 (11, "CosmicExpansion") shortestGalaxyPathsSum $ Just hugeExpansionGalaxyPathsSum,
      prettySolution2 (12, "HotSprings") possibleCombinationsSum $ Just unfoldedPossibleCombinationsSum,
      prettySolution2 (13, "PointOfIncidence") mirrorScoresSum $ Just mirrorSmudgeScoresSum,
      prettySolution2 (14, "ParabolicReflectorDish") platformBeamLoadsSum $ Just spinningPlatformBeamLoadsSum,
      prettySolution2 (15, "LensLibrary") initSequenceHashesSum $ Just lensBoxFocusingPowersSum,
      prettySolution2 (16, "TheFloorWillBeLava") energizedTilesCount $ Just maximumEnergizedTilesCountAllStarts,
      prettySolution2 (17, "ClumsyCrucible") minimumCrucibleHeatLoss $ Just minimumUltraCrucibleHeatLoss,
      prettySolution2 (18, "LavaductLagoon") lagoonArea $ Just lagoonAreaFixed,
      prettySolution2 (19, "Aplenty") acceptedPartRatingsSum $ Just acceptedPartRatingCombinationsSum,
      prettySolution2 (20, "PulsePropagation") cablesWarmUp $ Just machineTurnOnClicks,
      prettySolution2 (21, "StepCounter") gardenReachablePlotsCount $ Just infiniteGardenReachablePlotsCount,
      prettySolution2 (22, "SandSlabs") safeBricksCount $ Just unsafeBrickFallsCount,
      prettySolution2 (23, "ALongWalk") walkLongestHike $ Just walkLongestDryHike,
      prettySolution2 (24, "NeverTellMeTheOdds") hailstoneCollisionsCount $ Just perfectRockThrowCoordinatesSum,
      prettySolution2 (25, "Snowverload") splitComponentSizesProduct (Nothing :: Maybe (a -> a))
    ]

------------------------------------------------------------------------------------------------
-- Functions
prettySolution2 :: (Show a, Show b) => (Int, String) -> (String -> a) -> Maybe (String -> b) -> IO ()
prettySolution2 (day, puzzle) solution1 maybeSolution2 = do
  putStrLn (printf "Day %d: %s" day puzzle)
  prettySolution puzzle 1 solution1
  case maybeSolution2 of
    Just solution2 -> prettySolution puzzle 2 solution2
    Nothing -> putStr ""
  putStrLn ""

prettySolution :: forall a. (Show a) => String -> Int -> (String -> a) -> IO ()
prettySolution puzzle part solution = do
  putStr $ printf "%d -> " part
  print
    . liftA3
      bool
      ( error
          . printf "Wrong solution for %s part %d: expected %s, but got %s" puzzle (show part) (show expectedResult)
          . show
      )
      id
      ((== expectedResult) . show)
    . solution
    . resourceToString
    $ (puzzle ++ ".in")
  where
    expectedResult :: String
    expectedResult = case part of
      1 -> fst $ puzzleAnswers HsMS.! puzzle
      2 -> snd $ puzzleAnswers HsMS.! puzzle
      _ -> error "Invalid puzzle part number"
