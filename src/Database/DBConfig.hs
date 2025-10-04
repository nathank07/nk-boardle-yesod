{-# LANGUAGE OverloadedStrings #-}

module Database.DBConfig (boardleDB) where

import Database.PostgreSQL.Simple as PG

boardleDB :: PG.ConnectInfo
boardleDB = PG.defaultConnectInfo
    { PG.connectDatabase = "boardle"
    , PG.connectUser = "nathan"
    , PG.connectHost = if tcp then "localhost" else ""
    , PG.connectPort = if tcp then 5432 else 0
    } where tcp = False