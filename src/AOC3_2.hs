{-# LANGUAGE LambdaCase #-}

module AOC3_2(solve) where

import Control.Arrow ((&&&), (>>>))
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List (transpose, foldl')

solve :: String -> Int
solve = uncurry (*) . (oxygenGenerator &&& co2Scrubber) . map (map digitToInt) . lines

oxygenGenerator :: [[Int]] -> Int
oxygenGenerator = findNumber FindMost 1 0

co2Scrubber :: [[Int]] -> Int
co2Scrubber = findNumber FindLeast 0 0

data Goal = FindLeast
          | FindMost 
    deriving Eq

findNumber :: Goal -> Int -> Int -> [[Int]] -> Int
findNumber goal def n numbers =
    case numbers of 
        [num] -> toDec num
        nums -> 
            let bitCriteria = case mostCommonBitAt n numbers of
                    Just b -> if goal == FindLeast then flipBit b else b
                    Nothing -> def 
            in findNumber goal def (n + 1) $ filterBitAt n bitCriteria numbers

toDec :: [Int] -> Int
toDec = foldl' (\acc n -> acc * 2 + n) 0

filterBitAt :: Int -> Int -> [[Int]] -> [[Int]]
filterBitAt n bit = filter (\e -> e !! n == bit)

mostCommonBitAt :: Int -> [[Int]] -> Maybe Int
mostCommonBitAt n =
    mostCommon . map (flip (!!) n)

flipBit :: Int -> Int
flipBit = \case
    0 -> 1
    1 -> 0

mostCommon :: [Int] -> Maybe Int
mostCommon list = max' (flip count list) 0 1

max' :: Ord b => (a -> b) -> a -> a -> Maybe a
max' f a b
    | f a > f b = Just a
    | f a < f b = Just b
    | otherwise = Nothing

count :: Eq a => a -> [a] -> Int
count elem = length . filter ((==) elem)