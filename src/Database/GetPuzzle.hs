{-# LANGUAGE OverloadedStrings #-}

module Database.GetPuzzle where

import Database.DBConfig (boardleDB, PuzzleEntry(..))
import Database.PostgreSQL.Simple
import Control.Exception (bracket)

type PuzzleID = String


-- getFullPuzzleById :: PuzzleID -> IO (Maybe PuzzleEntry)
-- getFullPuzzleById pid = bracket (connect boardleDB) close $ \conn -> do
--     q <- query conn "SELECT * FROM Puzzles WHERE id = (?)" (Only pid) :: IO [PuzzleEntry]
--     return $ case q of
--             [puzzle] -> Just puzzle
--             _ -> Nothing