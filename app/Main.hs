module Main (main) where

import Database.GetPuzzle (getFullPuzzleById)

main :: IO ()
main = do 
    puzzle <- getFullPuzzleById "001XA"
    putStrLn $ show puzzle