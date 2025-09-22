module Main (main) where

import Database.LoadCsv

main :: IO ()
main = Database.LoadCsv.loadCsv
