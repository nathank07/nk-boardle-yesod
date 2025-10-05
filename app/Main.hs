module Main (main) where

import Boardle.Boardle (SAN(..), getGuesses, Answer(..), Guess(..))
import Database.GetPuzzle (getFullPuzzleById, Puzzle(..))

main :: IO ()
main = do 
    puzzle <- getFullPuzzleById "D4fJo"
    case puzzle of
        Nothing -> putStrLn "Puzzle not found or invalid"
        Just p -> do
            putStrLn $ show (pFen p)
            putStrLn $ show (pFirstMove p)
            showGuess (pSanSolution p)

showGuess :: [SAN] -> IO ()
showGuess sans = do
    putStrLn $ "Enter moves"
    moves <- getLine
    putStrLn $ show $ map SAN (words moves)
    let guesses = map (Unknown . SAN) (words moves) 
    let answer = map Answer sans
    putStrLn $ "Answer: " ++ show answer
    putStrLn $ "Guesses: " ++ show (getGuesses guesses answer)
    showGuess sans