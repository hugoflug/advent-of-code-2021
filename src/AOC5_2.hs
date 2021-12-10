module AOC5_2(solve) where

import qualified Data.Map as M
import Data.List.Split (splitOn)

solve :: String -> Int
solve = M.size
    . M.filter (> 1) 
    . foldr (M.unionWith (+) . points) M.empty
    . map parse
    . lines

parse :: String -> ((Int, Int), (Int, Int))
parse = both (both read . splitIn2 ",") . splitIn2 " -> "

points :: ((Int, Int), (Int, Int)) -> M.Map (Int, Int) Int
points ((f1, t1), (f2, t2)) = 
    M.fromList $ flip zip (repeat 1) $ zip' (range f1 f2) (range t1 t2)

zip' :: [a] -> [b] -> [(a, b)]
zip' a b = case (a, b) of
    ([a], b) -> replicate (length b) a `zip` b
    (a, [b]) -> a `zip` replicate (length a) b
    (a, b) -> a `zip` b

range :: Int -> Int -> [Int]
range a b = if b >= a then [a..b] else reverse [b..a]

splitIn2 :: Eq a => [a] -> [a] -> ([a], [a])
splitIn2 splitter input = case splitOn splitter input of [a, b] -> (a, b)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)
