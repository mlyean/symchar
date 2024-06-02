module Partition where

import Misc (factorial)

import Data.List (group, sortBy)
import Data.Ord (Down (Down), comparing)

type Composition = [Int]
type Partition = Composition
type CycleType = Partition
type Permutation = [Int]

sgn :: Partition -> Int
sgn lam = (-1) ^ (sum lam - length lam)

cycleType :: Permutation -> Partition
cycleType perm = sortBy (comparing Down) (helper [] 1)
 where
  helper seen k
    | k > length perm = []
    | k `elem` seen = helper seen (k + 1)
    | otherwise = length cyck : helper (seen ++ cyck) (k + 1)
   where
    getCycle start k = if k == start then [] else k : getCycle start (perm !! (k - 1))
    cyck = k : getCycle k (perm !! (k - 1))

transpose :: Partition -> Partition
transpose [] = []
transpose (lam@(x : _)) = map (\k -> length $ filter (>= k) lam) [1 .. x]

partitionsMaxWidth :: Int -> Int -> [Partition]
partitionsMaxWidth 0 _ = [[]]
partitionsMaxWidth n mx = concatMap (\i -> map (i :) (partitionsMaxWidth (n - i) (min i (n - i)))) [1 .. mx]

partitionsMaxLength :: Int -> Int -> [Partition]
partitionsMaxLength n mx = map transpose $ partitionsMaxWidth n mx

partitions :: Int -> [Partition]
partitions n = partitionsMaxWidth n n

centralizerCard :: Partition -> Int
centralizerCard lam = product $ map (\(l@(x : xs)) -> x ^ (length l) * factorial (length l)) $ group lam

hookLength :: Partition -> (Int, Int) -> Int
hookLength lam (i, j) = (lam !! i) - j + (transpose lam !! j) - i - 1

legLength :: Partition -> (Int, Int) -> Int
legLength lam (i, j) = transpose lam !! j - i - 1

trimZeros :: Partition -> Partition
trimZeros = filter (> 0)

removeRim :: Partition -> (Int, Int) -> Partition
removeRim lam (i, j) = trimZeros (helper lam i j)
 where
  helper [] i j = []
  helper [x] 0 j = [min x j]
  helper (x : y : xs) 0 j = if y <= j then j : y : xs else y - 1 : helper (y : xs) 0 j
  helper (x : xs) i j = x : helper (xs) (i - 1) j

boxes :: Partition -> [(Int, Int)]
boxes lam = concatMap (\(i, l) -> map (\j -> (i, j)) l) $ zip [0 ..] a
 where
  a = map (\j -> [0 .. j - 1]) lam

cycTypeToPerm :: CycleType -> Permutation
cycTypeToPerm cyc = helper 1 cyc
 where
  helper n [] = []
  helper n (c : cs) = ([n + 1 .. n + c - 1] ++ [n]) ++ helper (n + c) cs
