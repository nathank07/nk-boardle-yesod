{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Database.DBConfig 
    ( boardleDB
    , PuzzleEntry(..)
    ) where

import Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)

data PuzzleEntry = PuzzleEntry
    { pePuzzleId :: String
    , peFEN :: String
    , peUCISolution :: String
    , peRating :: Int
    , peRatingDeviation :: Int
    , pePopularity :: Int
    , peThemes :: [String]
    } deriving (Show, Generic)

boardleDB :: PG.ConnectInfo
boardleDB = PG.defaultConnectInfo
    { PG.connectDatabase = "boardle"
    , PG.connectUser = "nathan"
    , PG.connectHost = if tcp then "localhost" else ""
    , PG.connectPort = if tcp then 5432 else 0
    } where tcp = False