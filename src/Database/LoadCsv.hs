{-# LANGUAGE OverloadedStrings #-}

module Database.LoadCsv 
    ( loadCsv
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Boardle.Boardle (getSANMoves')

data PuzzleRecord = PuzzleRecord
    { puzzleId :: !String
    , fen :: !String
    , moves :: !String
    , rating :: !Int
    , ratingDeviation :: !Int
    , popularity :: !Int
    , themes :: !String
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

loadCsv :: IO ()
loadCsv = do
    csvData <- BL.readFile "src/Database/lichess_db_puzzle.csv"
    case decodeByNameWithP valueParse defaultDecodeOptions csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            case getSANMoves' (fen p) (words $ moves p) of
                (Just x) -> case length x of
                    6 -> putStrLn $ unwords x
                    _ -> putStr ""
                _ -> putStrLn "Fail"
    putStrLn "Done"