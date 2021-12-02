{-# LANGUAGE LambdaCase #-}

module AOC2_1(solve) where

import Data.List.Split (splitOn)
import Data.List (foldl')

solve :: String -> Int
solve = uncurry (*) . foldl' move (0, 0) . map parseLine . lines

move :: (Int, Int) -> (String, Int) -> (Int, Int)
move (depth, pos) = \case
    ("forward", n) -> (depth, pos + n)
    ("up", n) -> (depth - n, pos)
    ("down", n) -> (depth + n, pos) 

parseLine :: String -> (String, Int)
parseLine = (head &&& read . last) . splitOn " "