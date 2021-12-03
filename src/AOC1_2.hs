{-# LANGUAGE TypeApplications #-}

module AOC1_2(solve) where

import Data.List (foldl')

solve :: String -> Int
solve input = snd . foldl' count (take 3 list, 0) . drop 3 $ list
    where list = map (read @Int) . lines $ input

count :: ([Int], Int) -> Int -> ([Int], Int)
count (prev, count) val = (curr, if sum curr > sum prev then count + 1 else count)
    where curr = drop 1 prev <> [val]