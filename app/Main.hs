module Main (main) where

import Lib

main :: IO ()
main = do
  print $ chi [3, 2, 2, 1, 1] [1, 1, 1, 1, 1, 1, 1, 1, 1]
