module Main where

import Lib

-- input = "test.csv"
input = "KEN_ALL.CSV"
output = "ken_all.converted.csv"

main :: IO ()
main = convert input output
