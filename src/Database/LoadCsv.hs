{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Database.LoadCsv
    ( loadCsvFileToPG
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Control.Exception (bracket)
import Database.DBConfig (boardleDB, PuzzleEntry(..))

data PuzzleRecord = PuzzleRecord
    { rePuzzleId :: !String
    , reFEN :: !String
    , reUCISolution :: !String
    , reRating :: !Int
    , reRatingDeviation :: !Int
    , rePopularity :: !Int
    , reThemes :: !String
    } deriving (Show)

valueParse :: NamedRecord -> Parser PuzzleRecord
valueParse r = PuzzleRecord
    <$> r .: "PuzzleId"
    <*> r .: "FEN"
    <*> r .: "Moves"
    <*> r .: "Rating"
    <*> r .: "RatingDeviation"
    <*> r .: "Popularity"
    <*> r .: "Themes"

addEntry :: Connection -> PuzzleEntry -> IO ()
addEntry conn (PuzzleEntry pid fen uci rating rdev pop themes) = do
    let q = "INSERT INTO Puzzles (id, fen, uci_solution, rating, rating_deviation, popularity) VALUES (?,?,?,?,?,?);"
    _ <- execute conn q (pid, fen, uci, rating, rdev, pop)
    mapM_ (\theme -> do
        let tq = "INSERT INTO Themes (name) VALUES (?) ON CONFLICT (name) DO NOTHING;"
        _ <- execute conn tq (Only theme)
        let tq2 = "SELECT id FROM Themes WHERE name = ?;"
        [Only themeId] <- query conn tq2 (Only theme) :: IO [Only Int]
        let tq3 = "INSERT INTO Puzzle_Themes (puzzle_id, theme_id) VALUES (?,?);"
        _ <- execute conn tq3 (pid, themeId)
        return ()) themes
    return ()

loadCsvFileToPG :: FilePath -> Int -> Int -> IO ()
loadCsvFileToPG csvFile minSquares maxSquares = do
    csvData <- BL.readFile csvFile
    case decodeByNameWithP valueParse defaultDecodeOptions csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            if squares p >= minSquares && squares p <= maxSquares
                then bracket (connect boardleDB) close $ \conn -> 
                            addEntry conn (PuzzleEntry
                                (rePuzzleId p)
                                (reFEN p)
                                (reUCISolution p)
                                (reRating p)
                                (reRatingDeviation p)
                                (rePopularity p)
                                (words $ reThemes p))
                else return ()
    putStrLn "Done inserting puzzles"
    where
        squares p = length $ filter (== ' ') (reFEN p)
