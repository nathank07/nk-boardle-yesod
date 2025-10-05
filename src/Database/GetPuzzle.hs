{-# LANGUAGE OverloadedStrings #-}

module Database.GetPuzzle 
    ( getFullPuzzleById
    , getPuzzleThemesByID
    , Puzzle(..)
    )where

import Database.DBConfig (boardleDB, PuzzleEntry(..))
import Database.PostgreSQL.Simple
import Control.Exception (bracket)
import Boardle.Boardle (getSANMoves', FEN(..), UCI(..), SAN(..))

type PuzzleID = String
type Theme = String

data Puzzle = Puzzle
    { pPuzzleId :: PuzzleID
    , pFen :: FEN
    , pSanSolution :: [SAN]
    , pFirstMove :: SAN
    , pRating :: Int
    , pRatingDeviation :: Int
    , pPopularity :: Int
    , pThemes :: [Theme]
    } deriving (Show)

getFullPuzzleById :: PuzzleID -> IO (Maybe Puzzle)
getFullPuzzleById pid = bracket (connect boardleDB) close $ \conn -> do
    q <- query conn "SELECT * FROM Puzzles WHERE id = (?)" (Only pid) :: IO [PuzzleEntry]
    themes <- getPuzzleThemesByID pid
    let q' = map (\p -> p { peThemes = themes }) q
    return $ case q' of
            [puzzle] -> 
                case getSANMoves' (FEN $ peFEN puzzle) (map UCI $ words $ peUCISolution puzzle) of
                    Just solution -> Just (Puzzle
                        (pePuzzleId puzzle)
                        (FEN $ peFEN puzzle)
                        (tail solution)
                        (head solution)
                        (peRating puzzle)
                        (peRatingDeviation puzzle)
                        (pePopularity puzzle)
                        (peThemes puzzle))
                    Nothing -> Nothing
            _ -> Nothing

getPuzzleThemesByID :: PuzzleID -> IO [Theme]
getPuzzleThemesByID pid = bracket (connect boardleDB) close $ \conn -> do
    q <- query conn "SELECT T.name \
                    \FROM Themes T \
                    \JOIN Puzzle_Themes PT \
                    \ON T.id = PT.theme_id \
                    \WHERE PT.puzzle_id = (?)" (Only pid) :: IO [Only Theme]
    return (map fromOnly q)
