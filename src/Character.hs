module Character where

import qualified Data.MemoCombinators as Memo
import Data.Ratio (numerator, (%))
import Partition

type Character = CycleType -> Int

innerProduct :: Int -> Character -> Character -> Int
innerProduct n chi1 chi2 = numerator $ sum $ map (\lam -> (chi1 lam * chi2 lam) % centralizerCard lam) (partitions n)

-- Compute irreducible characters using the Murnaghan-Nakayama Formula
chi' :: Partition -> Character
chi' [] _ = 1
chi' lam (c : cs) = sum (zipWith (*) sign a)
 where
  hooks = filter ((== c) . hookLength lam) $ boxes lam
  a = map (\p -> chi (removeRim lam p) cs) hooks
  sign = map (((-1) ^) . legLength lam) hooks
chi' lam c = error (show lam ++ " " ++ show c)

chi :: Partition -> Character
chi = Memo.memo2 (Memo.list Memo.integral) (Memo.list Memo.integral) chi'

characterTable :: Int -> [[Int]]
characterTable n = [[chi lam cyc | cyc <- partitions n] | lam <- partitions n]
