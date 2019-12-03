module Day2 where

import Data.Sequence (Seq(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import Data.List.Split
import Paths_aoc2019

handleOpCode :: Int -> Seq Int -> Seq Int
handleOpCode pos ops
  | op == 1   = handleOpCode newPos (Seq.update dest (in1 + in2) ops)
  | op == 2   = handleOpCode newPos (Seq.update dest (in1 * in2) ops)
  | otherwise = ops
  where
    op = ops `Seq.index` pos
    in1 = ops `Seq.index` (ops `Seq.index` (pos + 1))
    in2 = ops `Seq.index` (ops `Seq.index` (pos + 2))
    dest = ops `Seq.index` (pos + 3)
    newPos = pos + 4

testOp :: Int -> Seq Int -> Bool
testOp solution ops = solution == ((handleOpCode 0 ops) `Seq.index` 0)

findOp :: Int -> [[Int]] -> Seq Int -> Maybe Int
findOp _ [] _ = Nothing
findOp solution ([noun, verb]:rest) ops 
    | testOp solution newOps = Just (100 * noun + verb)
    | otherwise              = findOp solution rest ops
    where
        newOps = Seq.update 1 noun (Seq.update 2 verb ops)

execute = do
    filePath <- getDataFileName "inputs/day2-input.txt"
    opsFile <- readFile filePath
    let opsFileSplit = splitOn "," opsFile
    let ops = Seq.fromList (map read opsFileSplit)  -- Parse Strings as Ints
    let ops' = Seq.update 1 12 ops
    let ops'' = Seq.update 2 2 ops'
    let part1 = handleOpCode 0 ops''
    let part2 = findOp 19690720 (sequence [[0..99], [0..99]]) ops
    putStrLn ("Day 2")
    putStrLn ("Part 1:")
    print (Seq.index part1 0)
    putStrLn ("Part 2:")
    print (part2)
    putStrLn ("")