module Main where

import Lib

main :: IO ()
main = do
    getEnvironmentVars >>= buildAllHardwareQueryIO
    return ()
