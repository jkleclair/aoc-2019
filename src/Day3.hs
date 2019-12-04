module Day3 where

import Debug.Trace (trace)
import Data.List.Split
import Paths_aoc2019

parseDirection :: String -> (Float, Float)
parseDirection (direction:distance)
    | direction == 'R' = (distance', 0)
    | direction == 'U' = (0, distance')
    | direction == 'L' = (negate distance', 0)
    | direction == 'D' = (0, negate distance')
    where
        distance' = read distance :: Float

parseDirections :: [String] -> [(Float, Float)] -> [(Float, Float)]
parseDirections [] acc = acc
parseDirections (direction:rest) acc = parseDirections rest (acc ++ [(x1 + x2, y1 + y2)])
    where
        (x1, y1) = last acc
        (x2, y2) = parseDirection direction

-- https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
calcX :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Maybe (Float, Float)
calcX (x1, y1) (x2, y2) (x3, y3) (x4, y4)
   -- | trace "calcX" False = Nothing
   | denominator == 0 = Nothing
   -- | trace (show (x,y)) False = Nothing
   | (x, y) == (0, 0) = Nothing -- Don't count the starting point
   | not (0 <= t && t <= 1) || not (0 <= u && u <= 1) = Nothing
   | otherwise = Just (x, y)
   where
        (x, y) = (x1 + t * (x2 - x1), y1 + t * (y2 - y1))
        t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denominator
        u = negate (((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denominator)
        denominator = ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))

-- getIntersections :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
-- getIntersections (point:[]) _ = []
--   | Nothing    <- intersection = [] ++ getIntersections (point2:rest) [point3, point4]
--   | Just (x,y) <- intersection = [(x,y)] ++ getIntersections (point2:rest) [point3, point4]
--   where
--     intersection = calcX point1 point2 point3 point4

getIntersections :: [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
getIntersections _ _ (point:[]) = []
getIntersections origFirst (point:[]) (p:ps) = getIntersections origFirst origFirst ps
getIntersections origFirst (point1:point2:firstPoints) (point3:point4:secondPoints)
  -- | trace (show point1 ++ show point2 ++ show point3 ++ show point4) False = []
  | Nothing    <- intersection = [] ++ getIntersections origFirst (point2:firstPoints) (point3:point4:secondPoints)
  | Just (x,y) <- intersection = [(x,y)] ++ getIntersections origFirst (point2:firstPoints) (point3:point4:secondPoints)
  where
    intersection = calcX point1 point2 point3 point4
getIntersections _ _ _ = []

manhattanDistance :: (Float, Float) -> Float
manhattanDistance (x,y) = x + y

execute = do
  filePath <- getDataFileName "inputs/day3-input.txt"
  directionsStr <- readFile filePath
  let directionsPair = lines directionsStr
  let lineA = parseDirections (splitOn "," (head directionsPair)) [(0,0)]
  let lineB = parseDirections (splitOn "," (last directionsPair)) [(0,0)]
  let xs = getIntersections lineA lineA lineB
  let distances = map manhattanDistance xs
  putStrLn ("Day 3")
  putStrLn ("Part 1: ")
  print (minimum distances)