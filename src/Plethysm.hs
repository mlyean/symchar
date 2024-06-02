module Plethysm where

import Partition
import Character
import PermutationCharacter

import Data.List (sort, sortBy)
import Data.Ord (Down (Down), comparing)

revSortSign :: [Int] -> ([Int], Int)
revSortSign l = (map fst sorted, sgn $ cycleType perm)
 where
  sorted = sortBy (comparing (Down . fst)) (zip l [1 ..])
  perm = map snd sorted

isDecreasing :: [Int] -> Bool
isDecreasing [] = True
isDecreasing [_] = True
isDecreasing (x : y : xs) = x >= y && isDecreasing (y : xs)

toPartition :: [Int] -> Maybe (Partition, Int)
toPartition beta = if isDecreasing lambda && all (>= 0) lambda then Just (trimZeros lambda, sign) else Nothing
 where
  beta' = zipWith (-) beta [1 ..]
  (lambda', sign) = revSortSign beta'
  lambda = zipWith (+) lambda' [1 ..]

generalizedChi :: [Int] -> Character
generalizedChi alpha cyc = maybe 0 (\(lam, s) -> s * chi lam cyc) (toPartition alpha)

-- chooseRem n l returns a list of all pairs of lists (a, b) where the a is a subsequence of l and b is the remainder
chooseRem :: Int -> [a] -> [([a], [a])]
chooseRem 0 l = [([], l)]
chooseRem _ [] = []
chooseRem n (x : xs) = map (\(a, b) -> (x : a, b)) (chooseRem (n - 1) xs) ++ map (\(a, b) -> (a, x : b)) (chooseRem n xs)

setsOfSets :: Int -> Int -> [[[Int]]]
setsOfSets n m = helper n m [1 .. (n * m)]
 where
  helper 0 m l = [[]]
  helper n m (x : xs) = concatMap (\(a, b) -> map ((x : a) :) $ helper (n - 1) m b) $ chooseRem (m - 1) xs

a :: Int -> Int -> Partition -> Int
a n m lam = innerProduct (n * m) (permChar onSetsOfSets (setsOfSets n m)) (generalizedChi lam)
