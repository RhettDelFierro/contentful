module Main where

import Lib

main :: IO ()
main = do
    gs <- getAllGameIO
    print gs
