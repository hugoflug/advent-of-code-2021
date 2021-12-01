{-# LANGUAGE TypeApplications #-}

module AOC1_1(solve) where

import Data.List (foldl')

solve :: String -> Int
solve = snd . foldl' count (maxBound, 0) . map (read @Int) . lines
    where count (prev, count) val = (val, if val > prev then count + 1 else count)