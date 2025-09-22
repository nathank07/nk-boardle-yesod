{-# LANGUAGE OverloadedStrings #-}

module Database.LoadCsv 
    ( loadCsv
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Game.Chess
import Game.Chess.SAN
import Control.Monad (foldM)

getSANMoves :: Position -> [String] -> Maybe [String]
getSANMoves pos uciMoves = fmap (reverse . snd) $ foldM step (pos, []) uciMoves
    where
        step (currPos, sanMoves) uci =
            (\p -> (unsafeDoPly currPos p, toSAN currPos p : sanMoves))
                <$> fromUCI currPos uci


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
            case fromFEN (fen p) of
                Just pos -> case getSANMoves pos (words (moves p)) of
                    Just sanMoves -> putStr ""
                    Nothing -> putStrLn "Error converting moves"
                Nothing -> putStrLn "Error parsing FEN"
    putStrLn "Done"