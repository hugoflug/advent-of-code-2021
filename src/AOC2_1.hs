module AOC2_1(solve) where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import Data.List (foldl')

solve :: String -> Int
solve = uncurry (*) . foldl' move (0, 0) . map parseLine . lines

move :: (Int, Int) -> (String, Int) -> (Int, Int)
move (depth, pos) (instr, n) = case instr of
    "forward" -> (depth, pos + n)
    "up" -> (depth - n, pos)
    "down" -> (depth + n, pos) 

parseLine :: String -> (String, Int)
parseLine = (head &&& read . last) . splitOn " "