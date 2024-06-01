module Main (main) where

import Lib
import Control.Monad (forM_)

main :: IO ()
main = do
  n <- readLn
  let tbl = characterTable n
  forM_ tbl (\row -> print row)
