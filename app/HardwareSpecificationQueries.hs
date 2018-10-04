module Main where

import Lib

main :: IO ()
main = do
    ios <- getAllHardwareSpecificationIO
    print ios