{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.GetPuzzle 
    ( getFullPuzzleById
    , getPuzzleThemesByID
    , getRandomPuzzleID
    , getRandomPuzzleID'
    , Puzzle(..)
    , PuzzleSearch(..)
    , PuzzleID(..)
    , Theme(..)
    , Rating(..)
    )where

import System.Random (randomRIO)
import Database.DBConfig (boardleDB, PuzzleEntry(..))
import Database.PostgreSQL.Simple
import Control.Exception (bracket)
import Boardle.Boardle (getSANMoves', FEN(..), UCI(..), SAN(..))
import GHC.Generics (Generic)
import ClassyPrelude.Yesod (ToJSON, intercalate)
import Data.String (fromString)


newtype PuzzleID = PuzzleID String deriving (Show, Generic)
newtype Theme = Theme String deriving (Show, Generic)

type EloRating = Int
type RatingDeviation = Int

data Rating = Rating {
    rating :: EloRating,
    ratingDeviation :: RatingDeviation
} deriving (Show, Generic)

instance ToJSON PuzzleID
instance ToJSON Theme
instance ToJSON Rating

data Puzzle = Puzzle
    { pPuzzleId :: PuzzleID
    , pFen :: FEN
    , pSanSolution :: [SAN]
    , pFirstMove :: SAN
    , pRating :: Rating
    , pPopularity :: Int
    , pThemes :: [Theme]
    } deriving (Show, Generic)

data PuzzleSearch = PuzzleSearch
    { psMinRating :: Int
    , psMaxRating :: Int
    , psMaxRatingDeviation :: Int
    , psMinSquares :: Int
    , psMaxSquares :: Int
    , psThemes :: [Theme]
    } deriving (Show, Generic)

instance ToJSON Puzzle

getFullPuzzleById :: PuzzleID -> IO (Maybe Puzzle)
getFullPuzzleById (PuzzleID pid) = bracket (connect boardleDB) close $ \conn -> do
    q <- query conn "SELECT id, fen, uci_solution, rating, rating_deviation, popularity \
                    \FROM Puzzles WHERE id = (?)" (Only pid) :: IO [PuzzleEntry]
    themes <- getPuzzleThemesByID (PuzzleID pid)
    let themeStrings = map (\(Theme t) -> t) themes
        q' = map (\p -> p { peThemes = themeStrings }) q
    return $ case q' of
            [puzzle] -> 
                case getSANMoves' (FEN $ peFEN puzzle) (UCI <$> words (peUCISolution puzzle)) of
                    Just solution -> Just (Puzzle
                        (PuzzleID $ pePuzzleId puzzle)
                        (FEN $ peFEN puzzle)
                        (tail solution)
                        (head solution)
                        (Rating (peRating puzzle) (peRatingDeviation puzzle))
                        (pePopularity puzzle)
                        (map Theme (peThemes puzzle)))
                    Nothing -> Nothing
            _ -> Nothing

getPuzzleThemesByID :: PuzzleID -> IO [Theme]
getPuzzleThemesByID (PuzzleID pid) = bracket (connect boardleDB) close $ \conn -> do
    q <- query conn "SELECT T.name \
                    \FROM Themes T \
                    \JOIN Puzzle_Themes PT \
                    \ON T.id = PT.theme_id \
                    \WHERE PT.puzzle_id = (?)" (Only pid) :: IO [Only String]
    return (map (Theme . fromOnly) q)


getRandomPuzzleID :: PuzzleSearch -> IO (Maybe PuzzleID)
getRandomPuzzleID (PuzzleSearch lowRating highRating ratingDeviation lowSquares highSquares themes) =
    bracket (connect boardleDB) close $ \conn -> do
        r <- randomRIO (0.0, 1.0) :: IO Double

        let baseQuery = "SELECT P.id, P.random_val FROM Puzzles P"
            whereClause = "WHERE P.rating BETWEEN ? AND ?\
                          \ AND P.rating_deviation <= ?"
            themeJoin = if null themes
                then ""
                else " JOIN Puzzle_Themes PT ON P.id = PT.puzzle_id JOIN Themes T ON PT.theme_id = T.id"
            themeFilter = if null themes
                then ""
                else " AND T.name IN (" ++ intercalate "," (replicate (length themes) "?") ++ ")"
            squareFilter = if lowSquares <= highSquares
                then " AND LENGTH(P.uci_solution) - LENGTH(REPLACE(P.uci_solution, ' ', '')) BETWEEN ? AND ?"
                else ""

            sqlGE = baseQuery ++ themeJoin ++ " " ++ whereClause ++ themeFilter ++ squareFilter
                    ++ " AND P.random_val >= ? ORDER BY P.random_val LIMIT 1"

            sqlLT = baseQuery ++ themeJoin ++ " " ++ whereClause ++ themeFilter ++ squareFilter
                    ++ " AND P.random_val < ? ORDER BY P.random_val DESC LIMIT 1"

            baseParams = [show lowRating, show highRating, show ratingDeviation]

            themeParams = if null themes then [] else map (\(Theme t) -> t) themes

            squareParams = if lowSquares <= highSquares 
                           then [show lowSquares, show highSquares] else []

            allParams = baseParams ++ themeParams ++ squareParams ++ [show r]

        q1 <- query conn (fromString sqlGE) allParams :: IO [(String, Double)]
        -- wrap-around if no results found
        q2 <- query conn (fromString sqlLT) allParams :: IO [(String, Double)]

        case (q1, q2) of
            ([], [])    -> return Nothing
            (x:_, _)    -> return $ Just (PuzzleID $ fst x)
            ([], y:_)   -> return $ Just (PuzzleID $ fst y)


getRandomPuzzleID' :: IO (Maybe PuzzleID)
getRandomPuzzleID' = bracket (connect boardleDB) close $ \conn -> do
    r <- randomRIO (0.0, 1.0) :: IO Double

    q1 <- query conn
            "SELECT id, random_val FROM Puzzles WHERE random_val >= ? ORDER BY random_val LIMIT 1"
            (Only r) :: IO [(String, Double)]

    -- wrap-around if no results found
    q2 <- query conn
            "SELECT id, random_val FROM Puzzles WHERE random_val < ? ORDER BY random_val DESC LIMIT 1"
            (Only r) :: IO [(String, Double)]

    case (q1, q2) of 
        ([], []) -> return Nothing
        (x:_, _) -> return $ Just (PuzzleID $ fst x)
        ([], y:_) -> return $ Just (PuzzleID $ fst y)