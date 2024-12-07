{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -with-rtsopts=-N #-}

module Main
  ( main,
    trebuchetSolutions,
    cubeConundrumSolutions,
    gearRatiosSolutions,
    scratchcardsSolutions,
    ifYouGiveASeedAFertilizerSolutions,
    waitForItSolutions,
    camelCardsSolutions,
    hauntedWastelandSolutions,
    mirageMaintenanceSolutions,
    pipeMazeSolutions,
    cosmicExpansionSolutions,
    hotSpringsSolutions,
    pointOfIncidenceSolutions,
    parabolicReflectorDishSolutions,
    lensLibrarySolutions,
    theFloorWillBeLavaSolutions,
    clumsyCrucibleSolutions,
    lavaductLagoonSolutions,
    aplentySolutions,
    pulsePropagationSolutions,
    stepCounterSolutions,
    sandSlabsSolutions,
    aLongWalkSolutions,
    neverTellMeTheOddsSolutions,
    snowverloadSolutions,
  )
where

import ALongWalk (walkLongestDryHike, walkLongestHike)
import Aplenty (acceptedPartRatingCombinations, acceptedPartRatings)
import CamelCards (handWinningsJokers, handWinningsNormal)
import ClumsyCrucible (minimumCrucibleHeatLoss, minimumUltraCrucibleHeatLoss)
import Control.Applicative (liftA3)
import Control.Arrow ((>>>))
import CosmicExpansion (hugeExpansionGalaxyPaths, shortestGalaxyPaths)
import CubeConundrum (CubeColor (..), fewestCubes, possibleGame)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedDir)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HsMS (fromList, (!))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, mapMaybe)
import GearRatios (gearRatios, partNumbers)
import HauntedWasteland (camelEscapeTime, ghostEscapeTime)
import HotSprings (possibleCombinations, possibleCombinationsUnfolded)
import IfYouGiveASeedAFertilizer (nearestSeed, nearestSeedRange)
import LavaductLagoon (lagoonArea, lagoonAreaFixed)
import LensLibrary (initSequenceHashes, lensBoxFocusingPowers)
import MirageMaintenance (initialValuePrediction, nextValuePrediction)
import NeverTellMeTheOdds (Hailstone (..), hailstoneCollisions, throwPerfectRock)
import ParabolicReflectorDish (platformBeamLoads, spinningPlatformBeamLoads)
import PipeMaze (farthestPipeSteps, nestPipesCount)
import PointOfIncidence (mirrorScore, mirrorSmudgeScore)
import PulsePropagation (cablesWarmUp, machineTurnOnClicks)
import SandSlabs (safeBricksCount, unsafeBrickFallsCount)
import Scratchcards (scratchcardCloneCounts, scratchcardPoints)
import Snowverload (splitComponentSizes)
import StepCounter (gardenReachablePlotsCount, infiniteGardenReachablePlotsCount)
import Text.Printf (printf)
import TheFloorWillBeLava (energizedTilesCount, energizedTilesCountAllStarts)
import Trebuchet (retrieveCalibration, retrieveCalibrationFixed)
import WaitForIt (waysToRecord, waysToRecordFullRace)

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
    . foldr
      ( \s ls ->
          putStrLn
            (printf "Day %d" (25 - length ls `div` 3))
            : s
            : putStrLn ""
            : ls
      )
      []
    $ [ trebuchetSolutions,
        cubeConundrumSolutions,
        gearRatiosSolutions,
        scratchcardsSolutions,
        ifYouGiveASeedAFertilizerSolutions,
        waitForItSolutions,
        camelCardsSolutions,
        hauntedWastelandSolutions,
        mirageMaintenanceSolutions,
        pipeMazeSolutions,
        cosmicExpansionSolutions,
        hotSpringsSolutions,
        pointOfIncidenceSolutions,
        parabolicReflectorDishSolutions,
        lensLibrarySolutions,
        theFloorWillBeLavaSolutions,
        clumsyCrucibleSolutions,
        lavaductLagoonSolutions,
        aplentySolutions,
        pulsePropagationSolutions,
        stepCounterSolutions,
        sandSlabsSolutions,
        aLongWalkSolutions,
        neverTellMeTheOddsSolutions,
        snowverloadSolutions
      ]

trebuchetSolutions :: IO ()
trebuchetSolutions =
  prettySolution2
    "Trebuchet"
    (sum . map retrieveCalibration . lines)
    (sum . map retrieveCalibrationFixed . lines)

cubeConundrumSolutions :: IO ()
cubeConundrumSolutions =
  prettySolution2
    "CubeConundrum"
    (sum . mapMaybe (possibleGame [(Blue, 14), (Green, 13), (Red, 12)]) . lines)
    (sum . map (product . map snd . fewestCubes) . lines)

gearRatiosSolutions :: IO ()
gearRatiosSolutions =
  prettySolution2
    "GearRatios"
    (sum . partNumbers)
    (sum . gearRatios)

scratchcardsSolutions :: IO ()
scratchcardsSolutions =
  prettySolution2
    "Scratchcards"
    (sum . map scratchcardPoints . lines)
    (sum . scratchcardCloneCounts)

ifYouGiveASeedAFertilizerSolutions :: IO ()
ifYouGiveASeedAFertilizerSolutions =
  prettySolution2
    "IfYouGiveASeedAFertilizer"
    nearestSeed
    nearestSeedRange

waitForItSolutions :: IO ()
waitForItSolutions =
  prettySolution2
    "WaitForIt"
    (product . waysToRecord)
    waysToRecordFullRace

camelCardsSolutions :: IO ()
camelCardsSolutions =
  prettySolution2
    "CamelCards"
    (sum . handWinningsNormal)
    (sum . handWinningsJokers)

hauntedWastelandSolutions :: IO ()
hauntedWastelandSolutions =
  prettySolution2
    "HauntedWasteland"
    camelEscapeTime
    ghostEscapeTime

mirageMaintenanceSolutions :: IO ()
mirageMaintenanceSolutions =
  prettySolution2
    "MirageMaintenance"
    (sum . map nextValuePrediction . lines)
    (sum . map initialValuePrediction . lines)

pipeMazeSolutions :: IO ()
pipeMazeSolutions =
  prettySolution2
    "PipeMaze"
    farthestPipeSteps
    nestPipesCount

cosmicExpansionSolutions :: IO ()
cosmicExpansionSolutions =
  prettySolution2
    "CosmicExpansion"
    (sum . shortestGalaxyPaths)
    (sum . hugeExpansionGalaxyPaths)

hotSpringsSolutions :: IO ()
hotSpringsSolutions =
  prettySolution2
    "HotSprings"
    (sum . map possibleCombinations . lines)
    (sum . map (possibleCombinationsUnfolded 5) . lines)

pointOfIncidenceSolutions :: IO ()
pointOfIncidenceSolutions =
  prettySolution2
    "PointOfIncidence"
    (sum . map mirrorScore . splitOn "\n\n")
    (sum . map mirrorSmudgeScore . splitOn "\n\n")

parabolicReflectorDishSolutions :: IO ()
parabolicReflectorDishSolutions =
  prettySolution2
    "ParabolicReflectorDish"
    (sum . platformBeamLoads)
    (sum . spinningPlatformBeamLoads 1000000000)

lensLibrarySolutions :: IO ()
lensLibrarySolutions =
  prettySolution2
    "LensLibrary"
    (sum . initSequenceHashes)
    (sum . lensBoxFocusingPowers)

theFloorWillBeLavaSolutions :: IO ()
theFloorWillBeLavaSolutions =
  prettySolution2
    "TheFloorWillBeLava"
    energizedTilesCount
    (maximum . energizedTilesCountAllStarts)

clumsyCrucibleSolutions :: IO ()
clumsyCrucibleSolutions =
  prettySolution2
    "ClumsyCrucible"
    minimumCrucibleHeatLoss
    minimumUltraCrucibleHeatLoss

lavaductLagoonSolutions :: IO ()
lavaductLagoonSolutions =
  prettySolution2
    "LavaductLagoon"
    lagoonArea
    lagoonAreaFixed

aplentySolutions :: IO ()
aplentySolutions =
  prettySolution2
    "Aplenty"
    (sum . acceptedPartRatings)
    (sum . acceptedPartRatingCombinations)

pulsePropagationSolutions :: IO ()
pulsePropagationSolutions =
  prettySolution2
    "PulsePropagation"
    cablesWarmUp
    machineTurnOnClicks

stepCounterSolutions :: IO ()
stepCounterSolutions =
  prettySolution2
    "StepCounter"
    (gardenReachablePlotsCount 64)
    (infiniteGardenReachablePlotsCount 26501365)

sandSlabsSolutions :: IO ()
sandSlabsSolutions =
  prettySolution2
    "SandSlabs"
    safeBricksCount
    unsafeBrickFallsCount

aLongWalkSolutions :: IO ()
aLongWalkSolutions =
  prettySolution2
    "ALongWalk"
    walkLongestHike
    walkLongestDryHike

neverTellMeTheOddsSolutions :: IO ()
neverTellMeTheOddsSolutions =
  prettySolution2
    "NeverTellMeTheOdds"
    (length . hailstoneCollisions)
    ((round :: Double -> Int) . sum . position . throwPerfectRock)

snowverloadSolutions :: IO ()
snowverloadSolutions =
  prettySolution
    (1, "Snowverload")
    (uncurry (*) . splitComponentSizes)

------------------------------------------------------------------------------------------------
-- Functions

prettySolution2 :: (Show a, Show b) => String -> (String -> a) -> (String -> b) -> IO ()
prettySolution2 puzzle solution1 solution2 = do
  prettySolution (1, puzzle) solution1
  prettySolution (2, puzzle) solution2

prettySolution :: forall a. (Show a) => (Int, String) -> (String -> a) -> IO ()
prettySolution (part, puzzle) solution = do
  putStr $ printf "%d. %s -> " part puzzle
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
