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
import CosmicExpansion (hugeExpansionGalaxyPaths, shortestGalaxyPaths)
import CubeConundrum (CubeColor (..), fewestCubes, possibleGame)
import Data.Bool (bool)
import Data.Foldable (sequenceA_)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
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
    (sum . map retrieveCalibration . lines, 54388)
    (sum . map retrieveCalibrationFixed . lines, 53515)

cubeConundrumSolutions :: IO ()
cubeConundrumSolutions =
  prettySolution2
    "CubeConundrum"
    (sum . mapMaybe (possibleGame [(Blue, 14), (Green, 13), (Red, 12)]) . lines, 2278)
    (sum . map (product . map snd . fewestCubes) . lines, 67953)

gearRatiosSolutions :: IO ()
gearRatiosSolutions =
  prettySolution2
    "GearRatios"
    (sum . partNumbers, 520019)
    (sum . gearRatios, 75519888)

scratchcardsSolutions :: IO ()
scratchcardsSolutions =
  prettySolution2
    "Scratchcards"
    (sum . map scratchcardPoints . lines, 20117)
    (sum . scratchcardCloneCounts, 13768818)

ifYouGiveASeedAFertilizerSolutions :: IO ()
ifYouGiveASeedAFertilizerSolutions =
  prettySolution2
    "IfYouGiveASeedAFertilizer"
    (nearestSeed, 240320250)
    (nearestSeedRange, 28580589)

waitForItSolutions :: IO ()
waitForItSolutions =
  prettySolution2
    "WaitForIt"
    (product . waysToRecord, 4403592)
    (waysToRecordFullRace, 38017587)

camelCardsSolutions :: IO ()
camelCardsSolutions =
  prettySolution2
    "CamelCards"
    (sum . handWinningsNormal, 250602641)
    (sum . handWinningsJokers, 251037509)

hauntedWastelandSolutions :: IO ()
hauntedWastelandSolutions =
  prettySolution2
    "HauntedWasteland"
    (camelEscapeTime, 15989)
    (ghostEscapeTime, 13830919117339)

mirageMaintenanceSolutions :: IO ()
mirageMaintenanceSolutions =
  prettySolution2
    "MirageMaintenance"
    (sum . map nextValuePrediction . lines, 1702218515)
    (sum . map initialValuePrediction . lines, 925)

pipeMazeSolutions :: IO ()
pipeMazeSolutions =
  prettySolution2
    "PipeMaze"
    (farthestPipeSteps, 7102)
    (nestPipesCount, 363)

cosmicExpansionSolutions :: IO ()
cosmicExpansionSolutions =
  prettySolution2
    "CosmicExpansion"
    (sum . shortestGalaxyPaths, 9556712)
    (sum . hugeExpansionGalaxyPaths, 678626199476)

hotSpringsSolutions :: IO ()
hotSpringsSolutions =
  prettySolution2
    "HotSprings"
    (sum . map possibleCombinations . lines, 7653)
    (sum . map (possibleCombinationsUnfolded 5) . lines, 60681419004564)

pointOfIncidenceSolutions :: IO ()
pointOfIncidenceSolutions =
  prettySolution2
    "PointOfIncidence"
    (sum . map mirrorScore . splitOn "\n\n", 29846)
    (sum . map mirrorSmudgeScore . splitOn "\n\n", 25401)

parabolicReflectorDishSolutions :: IO ()
parabolicReflectorDishSolutions =
  prettySolution2
    "ParabolicReflectorDish"
    (sum . platformBeamLoads, 110677)
    (sum . spinningPlatformBeamLoads 1000000000, 90551)

lensLibrarySolutions :: IO ()
lensLibrarySolutions =
  prettySolution2
    "LensLibrary"
    (sum . initSequenceHashes, 517965)
    (sum . lensBoxFocusingPowers, 267372)

theFloorWillBeLavaSolutions :: IO ()
theFloorWillBeLavaSolutions =
  prettySolution2
    "TheFloorWillBeLava"
    (energizedTilesCount, 7860)
    (maximum . energizedTilesCountAllStarts, 8331)

clumsyCrucibleSolutions :: IO ()
clumsyCrucibleSolutions =
  prettySolution2
    "ClumsyCrucible"
    (minimumCrucibleHeatLoss, 817)
    (minimumUltraCrucibleHeatLoss, 925)

lavaductLagoonSolutions :: IO ()
lavaductLagoonSolutions =
  prettySolution2
    "LavaductLagoon"
    (lagoonArea, 40714)
    (lagoonAreaFixed, 129849166997110)

aplentySolutions :: IO ()
aplentySolutions =
  prettySolution2
    "Aplenty"
    (sum . acceptedPartRatings, 418498)
    (sum . acceptedPartRatingCombinations, 123331556462603)

pulsePropagationSolutions :: IO ()
pulsePropagationSolutions =
  prettySolution2
    "PulsePropagation"
    (cablesWarmUp, 899848294)
    (machineTurnOnClicks, 247454898168563)

stepCounterSolutions :: IO ()
stepCounterSolutions =
  prettySolution2
    "StepCounter"
    (gardenReachablePlotsCount 64, 3585)
    (infiniteGardenReachablePlotsCount 26501365, 597102953699891)

sandSlabsSolutions :: IO ()
sandSlabsSolutions =
  prettySolution2
    "SandSlabs"
    (safeBricksCount, 441)
    (unsafeBrickFallsCount, 80778)

aLongWalkSolutions :: IO ()
aLongWalkSolutions =
  prettySolution2
    "ALongWalk"
    (walkLongestHike, 2366)
    (walkLongestDryHike, 6682)

neverTellMeTheOddsSolutions :: IO ()
neverTellMeTheOddsSolutions =
  prettySolution2
    "NeverTellMeTheOdds"
    (length . hailstoneCollisions, 21679)
    ((round :: Double -> Int) . sum . position . throwPerfectRock, 566914635762564)

snowverloadSolutions :: IO ()
snowverloadSolutions =
  prettySolution
    (1, "Snowverload")
    (uncurry (*) . splitComponentSizes, 545528)

------------------------------------------------------------------------------------------------
-- Functions

prettySolution2 :: (Show a, Show b, Eq a, Eq b) => String -> (String -> a, a) -> (String -> b, b) -> IO ()
prettySolution2 puzzle (solution1, expectedResult1) (solution2, expectedResult2) = do
  prettySolution (1, puzzle) (solution1, expectedResult1)
  prettySolution (2, puzzle) (solution2, expectedResult2)

prettySolution :: (Show a, Eq a) => (Int, String) -> (String -> a, a) -> IO ()
prettySolution (part, puzzle) (solution, expectedResult) = do
  putStr $ printf "%d. %s -> " part puzzle
  print
    . liftA3
      bool
      ( error
          . printf "Wrong solution for %s part %d: expected %s, but got %s" puzzle part (show expectedResult)
          . show
      )
      id
      (== expectedResult)
    . solution
    =<< readFile (printf "src/resources/%s.in" puzzle)