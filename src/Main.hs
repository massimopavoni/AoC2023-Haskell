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
  )
where

import CamelCards (handWinningsJokers, handWinningsNormal)
import Control.Exception (assert)
import CosmicExpansion (hugeExpansionGalaxyPaths, shortestGalaxyPaths)
import CubeConundrum (CubeColor (..), fewestCubes, possibleGame)
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import GearRatios (gearRatios, partNumbers)
import HauntedWasteland (camelEscapeTime, ghostEscapeTime)
import IfYouGiveASeedAFertilizer (nearestSeed, nearestSeedRange)
import MirageMaintenance (initialValuePrediction, nextValuePrediction)
import PipeMaze (farthestPipeSteps, nestPipesCount)
import Scratchcards (scratchcardPoints, scratchcardsClonesCounts)
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
        cosmicExpansionSolutions
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

------------------------------------------------------------------------------------------------
-- Functions

solutionPretty :: (Show b, Eq b) => (String, Int) -> (String -> b, b) -> FilePath -> IO ()
solutionPretty (puzzle, part) (solution, expectedResult) source = do
  putStr $ unwords [puzzle, show part, "->"] ++ " "
  print . (assert =<< (== expectedResult)) . solution =<< readFile source