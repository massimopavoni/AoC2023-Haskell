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
  )
where

import Aplenty (acceptedPartRatingCombinations, acceptedPartRatings)
import CamelCards (handWinningsJokers, handWinningsNormal)
import ClumsyCrucible (minimumCrucibleHeatLoss, minimumUltraCrucibleHeatLoss)
import Control.Exception (assert)
import CosmicExpansion (hugeExpansionGalaxyPaths, shortestGalaxyPaths)
import CubeConundrum (CubeColor (..), fewestCubes, possibleGame)
import Data.List (intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import GearRatios (gearRatios, partNumbers)
import HauntedWasteland (camelEscapeTime, ghostEscapeTime)
import HotSprings (possibleCombinations, possibleCombinationsUnfolded)
import IfYouGiveASeedAFertilizer (nearestSeed, nearestSeedRange)
import LavaductLagoon (lagoonArea, lagoonAreaFixed)
import LensLibrary (initSequenceHashes, lensBoxFocusingPowers)
import MirageMaintenance (initialValuePrediction, nextValuePrediction)
import ParabolicReflectorDish (platformBeamLoads, spinningPlatformBeamLoads)
import PipeMaze (farthestPipeSteps, nestPipesCount)
import PointOfIncidence (mirrorScore, mirrorSmudgeScore)
import PulsePropagation (cablesWarmUp, machineTurnOnClicks)
import Scratchcards (scratchcardCloneCounts, scratchcardPoints)
import TheFloorWillBeLava (energizedTilesCount, energizedTilesCountAllStarts)
import Trebuchet (retrieveCalibration, retrieveCalibrationFixed)
import WaitForIt (waysToRecord, waysToRecordFullRace)

------------------------------------------------------------------------------------------------
-- Exports

main :: IO ()
main = do
  putStrLn "All solutions:\n"
  sequence_ $
    intersperse
      (putStrLn "")
      [ trebuchetSolutions,
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
        pulsePropagationSolutions
      ]

trebuchetSolutions :: IO ()
trebuchetSolutions = do
  solutionPretty
    ("Trebuchet", 1)
    (sum . map retrieveCalibration . lines, 54388)
  solutionPretty
    ("Trebuchet", 2)
    (sum . map retrieveCalibrationFixed . lines, 53515)

cubeConundrumSolutions :: IO ()
cubeConundrumSolutions = do
  solutionPretty
    ("CubeConundrum", 1)
    (sum . mapMaybe (possibleGame [(Blue, 14), (Green, 13), (Red, 12)]) . lines, 2278)
  solutionPretty
    ("CubeConundrum", 2)
    (sum . map (product . map snd . fewestCubes) . lines, 67953)

gearRatiosSolutions :: IO ()
gearRatiosSolutions = do
  solutionPretty
    ("GearRatios", 1)
    (sum . partNumbers, 520019)
  solutionPretty
    ("GearRatios", 2)
    (sum . gearRatios, 75519888)

scratchcardsSolutions :: IO ()
scratchcardsSolutions = do
  solutionPretty
    ("Scratchcards", 1)
    (sum . map scratchcardPoints . lines, 20117)
  solutionPretty
    ("Scratchcards", 2)
    (sum . scratchcardCloneCounts, 13768818)

ifYouGiveASeedAFertilizerSolutions :: IO ()
ifYouGiveASeedAFertilizerSolutions = do
  solutionPretty
    ("IfYouGiveASeedAFertilizer", 1)
    (nearestSeed, 240320250)
  solutionPretty
    ("IfYouGiveASeedAFertilizer", 2)
    (nearestSeedRange, 28580589)

waitForItSolutions :: IO ()
waitForItSolutions = do
  solutionPretty
    ("WaitForIt", 1)
    (product . waysToRecord, 4403592)
  solutionPretty
    ("WaitForIt", 2)
    (waysToRecordFullRace, 38017587)

camelCardsSolutions :: IO ()
camelCardsSolutions = do
  solutionPretty
    ("CamelCards", 1)
    (sum . handWinningsNormal, 250602641)
  solutionPretty
    ("CamelCards", 2)
    (sum . handWinningsJokers, 251037509)

hauntedWastelandSolutions :: IO ()
hauntedWastelandSolutions = do
  solutionPretty
    ("HauntedWasteland", 1)
    (camelEscapeTime, 15989)
  solutionPretty
    ("HauntedWasteland", 2)
    (ghostEscapeTime, 13830919117339)

mirageMaintenanceSolutions :: IO ()
mirageMaintenanceSolutions = do
  solutionPretty
    ("MirageMaintenance", 1)
    (sum . map nextValuePrediction . lines, 1702218515)
  solutionPretty
    ("MirageMaintenance", 2)
    (sum . map initialValuePrediction . lines, 925)

pipeMazeSolutions :: IO ()
pipeMazeSolutions = do
  solutionPretty
    ("PipeMaze", 1)
    (farthestPipeSteps, 7102)
  solutionPretty
    ("PipeMaze", 2)
    (nestPipesCount, 363)

cosmicExpansionSolutions :: IO ()
cosmicExpansionSolutions = do
  solutionPretty
    ("CosmicExpansion", 1)
    (sum . shortestGalaxyPaths, 9556712)
  solutionPretty
    ("CosmicExpansion", 2)
    (sum . hugeExpansionGalaxyPaths, 678626199476)

hotSpringsSolutions :: IO ()
hotSpringsSolutions = do
  solutionPretty
    ("HotSprings", 1)
    (sum . map possibleCombinations . lines, 7653)
  solutionPretty
    ("HotSprings", 2)
    (sum . map (possibleCombinationsUnfolded 5) . lines, 60681419004564)

pointOfIncidenceSolutions :: IO ()
pointOfIncidenceSolutions = do
  solutionPretty
    ("PointOfIncidence", 1)
    (sum . map mirrorScore . splitOn "\n\n", 29846)
  solutionPretty
    ("PointOfIncidence", 2)
    (sum . map mirrorSmudgeScore . splitOn "\n\n", 25401)

parabolicReflectorDishSolutions :: IO ()
parabolicReflectorDishSolutions = do
  solutionPretty
    ("ParabolicReflectorDish", 1)
    (sum . platformBeamLoads, 110677)
  solutionPretty
    ("ParabolicReflectorDish", 2)
    (sum . spinningPlatformBeamLoads 1000000000, 90551)

lensLibrarySolutions :: IO ()
lensLibrarySolutions = do
  solutionPretty
    ("LensLibrary", 1)
    (sum . initSequenceHashes, 517965)
  solutionPretty
    ("LensLibrary", 2)
    (sum . lensBoxFocusingPowers, 267372)

theFloorWillBeLavaSolutions :: IO ()
theFloorWillBeLavaSolutions = do
  solutionPretty
    ("TheFloorWillBeLava", 1)
    (energizedTilesCount, 7860)
  solutionPretty
    ("TheFloorWillBeLava", 2)
    (maximum . energizedTilesCountAllStarts, 8331)

clumsyCrucibleSolutions :: IO ()
clumsyCrucibleSolutions = do
  solutionPretty
    ("ClumsyCrucible", 1)
    (minimumCrucibleHeatLoss, 817)
  solutionPretty
    ("ClumsyCrucible", 2)
    (minimumUltraCrucibleHeatLoss, 925)

lavaductLagoonSolutions :: IO ()
lavaductLagoonSolutions = do
  solutionPretty
    ("LavaductLagoon", 1)
    (lagoonArea, 40714)
  solutionPretty
    ("LavaductLagoon", 2)
    (lagoonAreaFixed, 129849166997110)

aplentySolutions :: IO ()
aplentySolutions = do
  solutionPretty
    ("Aplenty", 1)
    (sum . acceptedPartRatings, 418498)
  solutionPretty
    ("Aplenty", 2)
    (sum . acceptedPartRatingCombinations, 123331556462603)

pulsePropagationSolutions :: IO ()
pulsePropagationSolutions = do
  solutionPretty
    ("PulsePropagation", 1)
    (cablesWarmUp, 899848294)
  solutionPretty
    ("PulsePropagation", 2)
    (machineTurnOnClicks, 247454898168563)

------------------------------------------------------------------------------------------------
-- Functions

solutionPretty :: (Show b, Eq b) => (String, Int) -> (String -> b, b) -> IO ()
solutionPretty (puzzle, part) (solution, expectedResult) = do
  putStr $ unwords [puzzle, show part, "->"] ++ " "
  print . (assert =<< (== expectedResult)) . solution =<< readFile ("src/resources/" ++ puzzle ++ ".in")