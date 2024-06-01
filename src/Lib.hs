module Lib (
    Composition,
    Partition,
    CycleType,
    Character,
    sgn,
    transpose,
    hookLength,
    legLength,
    trimZeros,
    removeRim,
    boxes,
    chi,
) where

type Composition = [Int]
type Partition = Composition
type CycleType = Partition
type Character = CycleType -> Int

sgn :: Partition -> Int
sgn lam = (-1) ^ (sum lam - length lam)

-- innerProduct :: Character -> Character -> Int
-- innerProduct = _

transpose :: Partition -> Partition
transpose [] = []
transpose (lam@(x : _)) = map (\k -> length $ filter (>= k) lam) [1 .. x]

hookLength :: Partition -> Int -> Int -> Int
hookLength lam i j = (lam !! i) - j + (transpose lam !! j) - i - 1

legLength :: Partition -> Int -> Int -> Int
legLength lam i j = transpose lam !! j - i - 1

trimZeros :: Partition -> Partition
trimZeros = filter (> 0)

removeRim :: Partition -> Int -> Int -> Partition
removeRim lam i j = trimZeros (helper lam i j)
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
chi :: Partition -> Character
chi [] _ = 1
chi lam (c : cs) = sum (zipWith (*) sgn a)
  where
    hooks = filter (\(i, j) -> hookLength lam i j == c) $ boxes lam
    a = map (\(i, j) -> chi (removeRim lam i j) cs) hooks
    sgn = map (\(i, j) -> (-1) ^ (legLength lam i j)) hooks
