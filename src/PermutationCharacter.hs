module PermutationCharacter where

import Character
import Partition
import Data.List (sort)

fixedPoints :: (Eq a) => (a -> a) -> [a] -> [a]
fixedPoints f = filter (\x -> f x == x)

permChar :: (Eq a) => (Permutation -> a -> a) -> [a] -> Character
permChar act l cyc = length $ fixedPoints (act $ cycTypeToPerm cyc) l

onSetsOfSets :: Permutation -> [[Int]] -> [[Int]]
onSetsOfSets perm = sort . map (sort . map (\x -> perm !! (x - 1)))
