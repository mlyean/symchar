module Lib (
  Composition,
  Partition,
  CycleType,
  Character,
  sgn,
  partitions,
  centralizerCard,
  innerProduct,
  transpose,
  hookLength,
  legLength,
  trimZeros,
  removeRim,
  boxes,
  chi,
  characterTable,
) where

import Data.List (group)
import qualified Data.MemoCombinators as Memo
import Data.Ratio (numerator, (%))

type Composition = [Int]
type Partition = Composition
type CycleType = Partition
type Character = CycleType -> Int

sgn :: Partition -> Int
sgn lam = (-1) ^ (sum lam - length lam)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

partitions :: Int -> [Partition]
partitions n = helper n n
 where
  helper 0 _ = [[]]
  helper n mx = concatMap (\i -> map (i :) (helper (n - i) (min i (n - i)))) [1 .. mx]

centralizerCard :: Partition -> Int
centralizerCard lam = product $ map (\(l@(x : xs)) -> x ^ (length l) * factorial (length l)) $ group lam

innerProduct :: Int -> Character -> Character -> Int
innerProduct n chi1 chi2 = numerator $ sum $ map (\lam -> (chi1 lam * chi2 lam) % centralizerCard lam) (partitions n)

transpose :: Partition -> Partition
transpose [] = []
transpose (lam@(x : _)) = map (\k -> length $ filter (>= k) lam) [1 .. x]

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

-- Compute irreducible characters using the Murnaghan-Nakayama Formula
chi' :: Partition -> Character
chi' [] _ = 1
chi' lam (c : cs) = sum (zipWith (*) sgn a)
 where
  hooks = filter ((== c) . hookLength lam) $ boxes lam
  a = map (\p -> chi (removeRim lam p) cs) hooks
  -- sgn = map (\p -> (-1) ^ (legLength lam p)) hooks
  sgn = map (((-1) ^) . legLength lam) hooks
chi' _ _ = undefined

chi :: Partition -> Character
chi = Memo.memo2 (Memo.list Memo.integral) (Memo.list Memo.integral) chi'

characterTable :: Int -> [[Int]]
characterTable n = [[chi lam cyc | cyc <- partitions n] | lam <- partitions n]
