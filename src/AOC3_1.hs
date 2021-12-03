{-# LANGUAGE LambdaCase #-}

module AOC3_1(solve) where

import Control.Arrow ((&&&), (>>>))
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List (transpose, foldl')

solve :: String -> Int
solve = uncurry (*) 
    . bimap toDec toDec 
    . (map leastCommon &&& map mostCommon)
    . transpose 
    . map (map digitToInt) 
    . lines

toDec :: [Int] -> Int
toDec = foldl' (\acc n -> acc * 2 + n) 0

leastCommon :: [Int] -> Int
leastCommon = flipBin . mostCommon

flipBin :: Int -> Int
flipBin = \case
    0 -> 1
    1 -> 0

mostCommon :: [Int] -> Int
mostCommon list = max' (flip count list) 0 1

max' :: Ord b => (a -> b) -> a -> a -> a
max' f a b = if f a > f b then a else b

count :: Eq a => a -> [a] -> Int
count elem = length . filter ((==) elem)