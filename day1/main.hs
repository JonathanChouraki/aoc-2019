module Main where

import Control.Exception
import Text.Printf

fuelRequirement :: Int -> Int
fuelRequirement mass = mass `div` 3 - 2

recursiveFuelRequirement :: Int -> Int
recursiveFuelRequirement m
  | requiredFuel <= 0 = 0
  | otherwise = requiredFuel  + (recursiveFuelRequirement requiredFuel)
  where requiredFuel = fuelRequirement m

solve :: (Int -> Int) -> [Int] -> Int
solve fn = sum . map fn

parseInput :: String -> IO [Int]
parseInput filepath = map read . lines <$> readFile filepath

main :: IO ()
main = do
  input <- parseInput "./input.txt"
  printf "part1: %d\n" $ solve fuelRequirement input
  printf "part2: %d\n" $ solve recursiveFuelRequirement input

runTest :: IO ()
runTest = do
  assert (fuelRequirement 12 == 2) $
    putStrLn "OK: fuelRequirement 12 == 2"
  assert (fuelRequirement 14 == 2) $
    putStrLn "OK: fuelRequirement 14 == 2"
  assert (fuelRequirement 1969 == 654) $
    putStrLn "OK: fuelRequirement 1969 == 654"
  assert (fuelRequirement 100756 == 33583) $
    putStrLn "OK: fuelRequirement 100756 == 33583"
  assert (recursiveFuelRequirement 12 == 2) $
    putStrLn "OK: recursiveFuelRequirement 12 == 2"
  assert (recursiveFuelRequirement 14 == 2) $
    putStrLn "OK: recursiveFuelRequirement 14 == 2"
  assert (recursiveFuelRequirement 1969 == 966) $
    putStrLn "OK: recursiveFuelRequirement 1969 == 966"
  assert (recursiveFuelRequirement 100756 == 50346) $
    putStrLn "OK: recursiveFuelRequirement 100756 == 50346"
