module Day1 where

import Text.Read (readMaybe)
import Paths_aoc2019

-- Part 1 Fuel Calculation
findFuel :: Maybe Int -> Int
findFuel Nothing = 0
findFuel (Just mass)
    | mass < 9  = 0     -- If fuel would be negative just return 0
    | otherwise = mass `div` 3 - 2

-- Part 2 Fuel Calculation
findFuel' :: Maybe Int -> Int
findFuel' Nothing = 0
findFuel' (Just mass)
    | mass < 9 = 0
    | otherwise = fuel + findFuel' (Just fuel)
    where
        fuel = mass `div` 3 - 2

execute = do
    filePath <- getDataFileName "inputs/day1-input.txt"
    massesFile <- readFile filePath
    let massesFileSplit = lines massesFile
    let masses = map readMaybe massesFileSplit  -- [Just x, Just y, ..., Just z]
    let fuels = map findFuel masses   -- Part 1 Calculation
    let fuels' = map findFuel' masses  -- Part 2 Calculation
    putStrLn ("Day 1")
    putStrLn ("Part 1:")
    print (sum fuels)
    putStrLn ("Part 2:")
    print (sum fuels')
    putStrLn ("")