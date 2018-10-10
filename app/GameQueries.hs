module Main where

import Lib

main :: IO ()
main = do
    gs <- results
    print gs