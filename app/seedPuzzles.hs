{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.IORef
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist
import Database.Persist.Postgresql
import Import.NoFoundation
import Model

main :: IO ()
main = do
    let csvFile = "lichess_db_puzzle.csv"
    loadCsvFileToPG csvFile 3 7

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
    <$> r Data.Csv..: "PuzzleId"
    <*> r Data.Csv..: "FEN"
    <*> r Data.Csv..: "Moves"
    <*> r Data.Csv..: "Rating"
    <*> r Data.Csv..: "RatingDeviation"
    <*> r Data.Csv..: "Popularity"
    <*> r Data.Csv..: "Themes"

-- Batch processing for much better performance
addEntryBatch :: ConnectionPool -> [PuzzleRecord] -> IO ()
addEntryBatch pool puzzleRecords = runSqlPool (do
    -- Pre-load all existing themes to avoid repeated lookups
    existingThemes <- selectList [] []
    let themeMap = Map.fromList [(themeName theme, entityKey entity) | entity@(Entity _ theme) <- existingThemes]
    themeMapRef <- liftIO $ Data.IORef.newIORef themeMap
    
    -- Process all puzzles in a single transaction
    forM_ puzzleRecords $ \puzzleRecord -> do
        let puzzleKey = PuzzleKey (pack $ rePuzzleId puzzleRecord)
        let uciSolution = words $ (pack $ reUCISolution puzzleRecord)

        case uciSolution of
            [] -> return ()
            (firstMove:remainingMoves) -> do
                insertKey puzzleKey $ Puzzle
                    (pack $ reFEN puzzleRecord)
                    firstMove
                    (unwords remainingMoves)
                    (Just $ fromIntegral $ length uciSolution - 1)
                    (fromIntegral $ reRating puzzleRecord)
                    (fromIntegral $ reRatingDeviation puzzleRecord)
                    (fromIntegral $ rePopularity puzzleRecord)
                    Nothing

                -- Handle themes efficiently with cached lookups
                let themeNames = map pack $ words $ reThemes puzzleRecord
                themeIds <- mapM (getOrCreateTheme themeMapRef) themeNames
                mapM_ (\themeId -> insert_ $ PuzzleTheme puzzleKey themeId) themeIds
    ) pool
  where
    getOrCreateTheme themeMapRef themeName = do
        currentMap <- liftIO $ Data.IORef.readIORef themeMapRef
        case Map.lookup themeName currentMap of
            Just themeId -> return themeId
            Nothing -> do
                themeId <- insert $ Theme themeName
                liftIO $ Data.IORef.modifyIORef themeMapRef (Map.insert themeName themeId)
                return themeId

-- Helper function to split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

type LowestSquares = Int
type HighestSquares = Int

loadCsvFileToPG :: FilePath -> LowestSquares -> HighestSquares -> IO ()
loadCsvFileToPG csvFile minSquares maxSquares = do
    -- You'll need to get the connection pool from your application
    -- This is a simplified version - you may need to adapt based on your app setup
    pool <- runStdoutLoggingT $ createPostgresqlPool 
        "host=localhost dbname=boardle user=boardle password=boardle" 1
    
    csvData <- BL.readFile csvFile
    case decodeByNameWithP valueParse defaultDecodeOptions csvData of
        Left err -> putStrLn $ pack err
        Right (_, v) -> do
            let validPuzzles = V.filter (\p -> squares p >= minSquares && squares p <= maxSquares) v
            let puzzleList = V.toList validPuzzles
            let batches = chunksOf 5000 puzzleList  -- Process in batches of 5000
            let totalBatches = length batches
                        
            forM_ (zip [1..] batches) $ \(batchNum, batch) -> do
                addEntryBatch pool batch
                
    putStrLn "Done inserting puzzles"
    where
        squares p = length $ filter (== ' ') (reUCISolution p)