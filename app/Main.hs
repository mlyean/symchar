{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString.Builder
import Data.List (intersperse)
import Lib
import System.IO (stdout)

main :: IO ()
main = do
  forever $ do
    n <- readLn
    let tbl = characterTable n
    hPutBuilder stdout $ mconcat [stringUtf8 "Character Table of S_", intDec n, stringUtf8 ":\n"]
    hPutBuilder stdout $ mconcat . intersperse (charUtf8 '\n') . map (mconcat . intersperse (stringUtf8 ", ") . map intDec) $ tbl
    hPutBuilder stdout $ charUtf8 '\n'
