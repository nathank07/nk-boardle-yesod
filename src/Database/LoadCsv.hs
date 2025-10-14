{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}

module Database.LoadCsv
    ( loadCsvFileToPG
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Database.PostgreSQL.Simple
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
addEntry conn (PuzzleEntry pid fen uci rating rdev pop themes) =
  withTransaction conn $ do
    let q =
          "INSERT INTO Puzzles \
          \(id, fen, uci_solution, rating, rating_deviation, popularity) \
          \VALUES (?,?,?,?,?,?);"
    _ <- execute conn q (pid, fen, uci, rating, rdev, pop)

    themeIds <- mapM (\theme -> do
      let tq =
            "INSERT INTO Themes (name) \
            \VALUES (?) \
            \ON CONFLICT (name) DO UPDATE SET name = EXCLUDED.name \
            \RETURNING id;"
      [Only tid] <- query conn tq (Only theme) :: IO [Only Int]
      return tid) themes

    let tq3 = "INSERT INTO Puzzle_Themes (puzzle_id, theme_id) VALUES (?,?)"
    _ <- executeMany conn tq3 [(pid, tid) | tid <- themeIds]

    return ()


type LowestSquares = Int
type HighestSquares = Int

loadCsvFileToPG :: FilePath -> LowestSquares -> HighestSquares -> IO ()
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
        squares p = length $ filter (== ' ') (reUCISolution p)
