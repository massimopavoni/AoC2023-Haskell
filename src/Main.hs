module Main (main, trebuchetSolution, cubeConundrumSolution) where

import CubeConundrum (CubeColor (..), fewestCubes, possibleGame)
import Data.Maybe (mapMaybe)
import GearRatios (partNumbers)
import Trebuchet (retrieveCalibration, retrieveCalibrationFixed)

main :: IO ()
main = do
  putStrLn "All solutions:\n"
  trebuchetSolution
  cubeConundrumSolution
  gearRatiosSolution

trebuchetSolution :: IO ()
trebuchetSolution = do
  putStr "Trebuchet one -> "
  print . sum . map retrieveCalibration . lines =<< readFile "src/resources/Trebuchet.in"
  putStr "Trebuchet two -> "
  print . sum . map retrieveCalibrationFixed . lines =<< readFile "src/resources/Trebuchet.in"
  putStrLn ""

cubeConundrumSolution :: IO ()
cubeConundrumSolution = do
  putStr "CubeConundrum one -> "
  print . sum . mapMaybe (possibleGame [(Blue, 14), (Green, 13), (Red, 12)]) . lines =<< readFile "src/resources/CubeConundrum.in"
  putStr "CubeConundrum two -> "
  print . sum . map (product . map snd . fewestCubes) . lines =<< readFile "src/resources/CubeConundrum.in"
  putStrLn ""

gearRatiosSolution :: IO ()
gearRatiosSolution = do
  putStr "GearRatios one -> "
  print . sum . partNumbers =<< readFile "src/resources/GearRatios.in"
  putStrLn ""
