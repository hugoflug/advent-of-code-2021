module AOC5_1(solve) where

import qualified Data.Map as M
import Data.List.Split (splitOn)

solve :: String -> Int
solve = M.size
    . M.filter (> 1) 
    . foldr (M.unionWith (+) . points) M.empty
    . filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2)
    . map parse
    . lines

parse :: String -> ((Int, Int), (Int, Int))
parse = both (both read . splitIn2 ",") . splitIn2 " -> "

points :: ((Int, Int), (Int, Int)) -> M.Map (Int, Int) Int
points ((f1, t1), (f2, t2)) = 
    M.fromList $ flip zip (repeat 1) $ (,) <$> ascRange f1 f2 <*> ascRange t1 t2

ascRange :: Int -> Int -> [Int]
ascRange a b = if b >= a then [a..b] else [b..a]

splitIn2 :: Eq a => [a] -> [a] -> ([a], [a])
splitIn2 splitter input = case splitOn splitter input of [a, b] -> (a, b)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)
