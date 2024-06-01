module Main (main) where

import Lib

main :: IO ()
main = do
  print $ characterTable 4
  print [[innerProduct 4 (chi lam) (chi mu) | lam <- partitions 4] | mu <- partitions 4]
