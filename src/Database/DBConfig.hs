{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Database.DBConfig 
    ( boardleDB
    , PuzzleEntry(..)
    ) where

import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow (fromRow, field)

data PuzzleEntry = PuzzleEntry
    { pePuzzleId :: String
    , peFEN :: String
    , peUCISolution :: String
    , peRating :: Int
    , peRatingDeviation :: Int
    , pePopularity :: Int
    , peThemes :: [String]
    } deriving (Show)

instance FromRow PuzzleEntry where
    fromRow = PuzzleEntry 
                <$> field 
                <*> field 
                <*> field 
                <*> field 
                <*> field 
                <*> field 
                <*> pure []  -- Themes populated seperately 

boardleDB :: PG.ConnectInfo
boardleDB = PG.defaultConnectInfo
    { PG.connectDatabase = "boardle"
    , PG.connectUser = "nathan"
    , PG.connectHost = if tcp then "localhost" else ""
    , PG.connectPort = if tcp then 5432 else 0
    } where tcp = False