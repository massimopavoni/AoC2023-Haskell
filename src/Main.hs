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
  )
where

import CamelCards (handWinningsJokers, handWinningsNormal)
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
import LensLibrary (initSequenceHashes, lensBoxFocusingPowers)
import MirageMaintenance (initialValuePrediction, nextValuePrediction)
import ParabolicReflectorDish (platformBeamLoads, spinningPlatformBeamLoads)
import PipeMaze (farthestPipeSteps, nestPipesCount)
import PointOfIncidence (mirrorScore, mirrorSmudgeScore)
import Scratchcards (scratchcardPoints, scratchcardsClonesCounts)
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
        theFloorWillBeLavaSolutions
      ]

trebuchetSolutions :: IO ()
trebuchetSolutions = do
  solutionPretty
    ("Trebuchet", 1)
    (sum . map retrieveCalibration . lines, 54388)
    "src/resources/Trebuchet.in"
  solutionPretty
    ("Trebuchet", 2)
    (sum . map retrieveCalibrationFixed . lines, 53515)
    "src/resources/Trebuchet.in"

cubeConundrumSolutions :: IO ()
cubeConundrumSolutions = do
  solutionPretty
    ("CubeConundrum", 1)
    (sum . mapMaybe (possibleGame [(Blue, 14), (Green, 13), (Red, 12)]) . lines, 2278)
    "src/resources/CubeConundrum.in"
  solutionPretty
    ("CubeConundrum", 2)
    (sum . map (product . map snd . fewestCubes) . lines, 67953)
    "src/resources/CubeConundrum.in"

gearRatiosSolutions :: IO ()
gearRatiosSolutions = do
  solutionPretty
    ("GearRatios", 1)
    (sum . partNumbers, 520019)
    "src/resources/GearRatios.in"
  solutionPretty
    ("GearRatios", 2)
    (sum . gearRatios, 75519888)
    "src/resources/GearRatios.in"

scratchcardsSolutions :: IO ()
scratchcardsSolutions = do
  solutionPretty
    ("Scratchcards", 1)
    (sum . map scratchcardPoints . lines, 20117)
    "src/resources/Scratchcards.in"
  solutionPretty
    ("Scratchcards", 2)
    (sum . scratchcardsClonesCounts, 13768818)
    "src/resources/Scratchcards.in"

ifYouGiveASeedAFertilizerSolutions :: IO ()
ifYouGiveASeedAFertilizerSolutions = do
  solutionPretty
    ("IfYouGiveASeedAFertilizer", 1)
    (nearestSeed, 240320250)
    "src/resources/IfYouGiveASeedAFertilizer.in"
  solutionPretty
    ("IfYouGiveASeedAFertilizer", 2)
    (nearestSeedRange, 28580589)
    "src/resources/IfYouGiveASeedAFertilizer.in"

waitForItSolutions :: IO ()
waitForItSolutions = do
  solutionPretty
    ("WaitForIt", 1)
    (product . waysToRecord, 4403592)
    "src/resources/WaitForIt.in"
  solutionPretty
    ("WaitForIt", 2)
    (waysToRecordFullRace, 38017587)
    "src/resources/WaitForIt.in"

camelCardsSolutions :: IO ()
camelCardsSolutions = do
  solutionPretty
    ("CamelCards", 1)
    (sum . handWinningsNormal, 250602641)
    "src/resources/CamelCards.in"
  solutionPretty
    ("CamelCards", 2)
    (sum . handWinningsJokers, 251037509)
    "src/resources/CamelCards.in"

hauntedWastelandSolutions :: IO ()
hauntedWastelandSolutions = do
  solutionPretty
    ("HauntedWasteland", 1)
    (camelEscapeTime, 15989)
    "src/resources/HauntedWasteland.in"
  solutionPretty
    ("HauntedWasteland", 2)
    (ghostEscapeTime, 13830919117339)
    "src/resources/HauntedWasteland.in"

mirageMaintenanceSolutions :: IO ()
mirageMaintenanceSolutions = do
  solutionPretty
    ("MirageMaintenance", 1)
    (sum . map nextValuePrediction . lines, 1702218515)
    "src/resources/MirageMaintenance.in"
  solutionPretty
    ("MirageMaintenance", 2)
    (sum . map initialValuePrediction . lines, 925)
    "src/resources/MirageMaintenance.in"

pipeMazeSolutions :: IO ()
pipeMazeSolutions = do
  solutionPretty
    ("PipeMaze", 1)
    (farthestPipeSteps, 7102)
    "src/resources/PipeMaze.in"
  solutionPretty
    ("PipeMaze", 2)
    (nestPipesCount, 363)
    "src/resources/PipeMaze.in"

cosmicExpansionSolutions :: IO ()
cosmicExpansionSolutions = do
  solutionPretty
    ("CosmicExpansion", 1)
    (sum . shortestGalaxyPaths, 9556712)
    "src/resources/CosmicExpansion.in"
  solutionPretty
    ("CosmicExpansion", 2)
    (sum . hugeExpansionGalaxyPaths, 678626199476)
    "src/resources/CosmicExpansion.in"

hotSpringsSolutions :: IO ()
hotSpringsSolutions = do
  solutionPretty
    ("HotSprings", 1)
    (sum . map possibleCombinations . lines, 7653)
    "src/resources/HotSprings.in"
  solutionPretty
    ("HotSprings", 2)
    (sum . map (possibleCombinationsUnfolded 5) . lines, 60681419004564)
    "src/resources/HotSprings.in"

pointOfIncidenceSolutions :: IO ()
pointOfIncidenceSolutions = do
  solutionPretty
    ("PointOfIncidence", 1)
    (sum . map mirrorScore . splitOn "\n\n", 29846)
    "src/resources/PointOfIncidence.in"
  solutionPretty
    ("PointOfIncidence", 2)
    (sum . map mirrorSmudgeScore . splitOn "\n\n", 25401)
    "src/resources/PointOfIncidence.in"

parabolicReflectorDishSolutions :: IO ()
parabolicReflectorDishSolutions = do
  solutionPretty
    ("ParabolicReflectorDish", 1)
    (sum . platformBeamLoads, 110677)
    "src/resources/ParabolicReflectorDish.in"
  solutionPretty
    ("ParabolicReflectorDish", 2)
    (sum . spinningPlatformBeamLoads 1000000000, 90551)
    "src/resources/ParabolicReflectorDish.in"

lensLibrarySolutions :: IO ()
lensLibrarySolutions = do
  solutionPretty
    ("LensLibrary", 1)
    (sum . initSequenceHashes, 517965)
    "src/resources/LensLibrary.in"
  solutionPretty
    ("LensLibrary", 2)
    (sum . lensBoxFocusingPowers, 267372)
    "src/resources/LensLibrary.in"

theFloorWillBeLavaSolutions :: IO ()
theFloorWillBeLavaSolutions = do
  solutionPretty
    ("TheFloorWillBeLava", 1)
    (energizedTilesCount, 7860)
    "src/resources/TheFloorWillBeLava.in"
  solutionPretty
    ("TheFloorWillBeLava", 2)
    (maximum . energizedTilesCountAllStarts, 8331)
    "src/resources/TheFloorWillBeLava.in"

------------------------------------------------------------------------------------------------
-- Functions

solutionPretty :: (Show b, Eq b) => (String, Int) -> (String -> b, b) -> FilePath -> IO ()
solutionPretty (puzzle, part) (solution, expectedResult) source = do
  putStr $ unwords [puzzle, show part, "->"] ++ " "
  print . (assert =<< (== expectedResult)) . solution =<< readFile source