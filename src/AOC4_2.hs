{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module AOC4_2(solve) where

import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Map ((!?))
import Data.List (maximumBy, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, fromJust)

solve :: String -> Int
solve input = 
    let (numbersToRounds, boards) = parse input
        timesToNumbers = inverseMap numbersToRounds
        (at, board) = maximumOn fst $ map (toFst $ earliestBingoForBoard numbersToRounds) boards
        in
            score numbersToRounds at board * (unNumber $ fromJust $ timesToNumbers !? at)

type Board = [[Number]]

newtype Number = Number { unNumber :: Int } deriving newtype (Eq, Ord, Read, Show)

newtype Round = Round { unRound :: Int } deriving newtype (Eq, Ord, Bounded, Show, Enum)

parse :: String -> (M.Map Number Round, [Board])
parse input = case splitOn "\n\n" input of
    head:tail -> (parseNumberList head, map parseBoard tail)

parseNumberList :: String -> M.Map Number Round
parseNumberList = M.fromList . flip zip [Round 0..] . map (read @Number) . splitOn ","

parseBoard :: String -> Board
parseBoard = map (map (read @Number) . words) . lines

score :: M.Map Number Round -> Round -> Board -> Int
score numbersToRounds at = sum . map scoreForNumber . concat
    where
        scoreForNumber n = case numbersToRounds !? n of
            Just time -> if time > at then unNumber n else 0
            Nothing -> 0

earliestBingoForBoard :: M.Map Number Round -> Board -> Round
earliestBingoForBoard numbersToRounds =
    minimum . map (earliestBingoForLine numbersToRounds) . uncurry (++) . toFst transpose

earliestBingoForLine :: M.Map Number Round -> [Number] -> Round
earliestBingoForLine numbersToRounds = maximum . map (numberToRound numbersToRounds)

numberToRound :: M.Map Number Round -> Number -> Round
numberToRound numbersToRounds = fromMaybe maxBound . (!?) numbersToRounds

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (\a b -> compare (f a) (f b))

dupe :: a -> (a,a)
dupe x = (x, x)

inverseMap :: (Ord k, Ord v) => M.Map k v -> M.Map v k
inverseMap m = M.fromList $ [(v, k) | (k, v) <- M.toList m]

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)