module Main
  ( main,
    trebuchetSolution,
    cubeConundrumSolution,
    gearRatiosSolution,
    scratchcardsSolution,
    ifYouGiveASeedAFertilizerSolution,
  )
where

import Control.Exception (assert)
import CubeConundrum (CubeColor (..), fewestCubes, possibleGame)
import Data.Maybe (mapMaybe)
import GearRatios (gearRatios, partNumbers)
import IfYouGiveASeedAFertilizer (nearestSeed, nearestSeedFixed)
import Scratchcards (scratchcardPoints, scratchcardsClonesCounts)
import Trebuchet (retrieveCalibration, retrieveCalibrationFixed)

main :: IO ()
main = do
  putStrLn "All solutions:\n"
  trebuchetSolution
  cubeConundrumSolution
  gearRatiosSolution
  scratchcardsSolution
  ifYouGiveASeedAFertilizerSolution

trebuchetSolution :: IO ()
trebuchetSolution = do
  solutionPretty
    ("Trebuchet", 1)
    (sum . map retrieveCalibration . lines, 54388)
    "src/resources/Trebuchet.in"
  solutionPretty
    ("Trebuchet", 2)
    (sum . map retrieveCalibrationFixed . lines, 53515)
    "src/resources/Trebuchet.in"
  putStrLn ""

cubeConundrumSolution :: IO ()
cubeConundrumSolution = do
  solutionPretty
    ("CubeConundrum", 1)
    (sum . mapMaybe (possibleGame [(Blue, 14), (Green, 13), (Red, 12)]) . lines, 2278)
    "src/resources/CubeConundrum.in"
  solutionPretty
    ("CubeConundrum", 2)
    (sum . map (product . map snd . fewestCubes) . lines, 67953)
    "src/resources/CubeConundrum.in"
  putStrLn ""

gearRatiosSolution :: IO ()
gearRatiosSolution = do
  solutionPretty
    ("GearRatios", 1)
    (sum . partNumbers, 520019)
    "src/resources/GearRatios.in"
  solutionPretty
    ("GearRatios", 2)
    (sum . gearRatios, 75519888)
    "src/resources/GearRatios.in"
  putStrLn ""

scratchcardsSolution :: IO ()
scratchcardsSolution = do
  solutionPretty
    ("Scratchcards", 1)
    (sum . map scratchcardPoints . lines, 20117)
    "src/resources/Scratchcards.in"
  solutionPretty
    ("Scratchcards", 2)
    (sum . scratchcardsClonesCounts, 13768818)
    "src/resources/Scratchcards.in"
  putStrLn ""

ifYouGiveASeedAFertilizerSolution :: IO ()
ifYouGiveASeedAFertilizerSolution = do
  solutionPretty
    ("IfYouGiveASeedAFertilizer", 1)
    (nearestSeed, 240320250)
    "src/resources/IfYouGiveASeedAFertilizer.in"
  solutionPretty
    ("IfYouGiveASeedAFertilizer", 2)
    (nearestSeedFixed, 28580589)
    "src/resources/IfYouGiveASeedAFertilizer.in"
  putStrLn ""

solutionPretty :: (Show b, Eq b) => (String, Int) -> (String -> b, b) -> FilePath -> IO ()
solutionPretty (puzzle, part) (solution, expectedResult) source = do
  putStr $ unwords [puzzle, show part, "->"] ++ " "
  print . (assert =<< (== expectedResult)) . solution =<< readFile source