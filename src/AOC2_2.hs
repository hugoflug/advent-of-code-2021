{-# LANGUAGE LambdaCase #-}

module AOC2_2(solve) where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import Data.List (foldl')

solve :: String -> Int
solve = (\(_, depth, pos) -> depth * pos)  . foldl' move (0, 0, 0) . map parseLine . lines

move :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
move (aim, depth, pos) = \case
    ("forward", n) -> (aim, depth + n*aim, pos + n)
    ("up", n) -> (aim - n, depth, pos)
    ("down", n) -> (aim + n, depth, pos) 

parseLine :: String -> (String, Int)
parseLine = (head &&& read . last) . splitOn " "