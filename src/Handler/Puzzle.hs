{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Puzzle where

import Import
import System.Random (randomRIO)
import Model
import Boardle.Boardle
import qualified Database.Esqueleto.Experimental as E
import Database.Esqueleto.Experimental ((^.))

getPuzzleIdR :: Text -> Handler Value
getPuzzleIdR puzzleId = do
    mpuzzle <- runDB $ get (PuzzleKey puzzleId)
    case mpuzzle of
        Nothing -> notFound
        Just (Puzzle fen firstMove solution _ rating ratingDev pop _) -> do 
            -- Grab themes related to puzzle
            themeIds <- runDB $ selectList [PuzzleThemePuzzleId ==. PuzzleKey puzzleId] []
            themes <- forM themeIds $ \(Entity _ puzzleTheme) -> do
                        mtheme <- runDB $ get (puzzleThemeThemeId puzzleTheme)
                        case mtheme of
                            Nothing -> return ""
                            Just theme -> return (themeName theme)

            let sanMoves = getSANMoves' (FEN fen) (UCI <$> (firstMove : words solution))

            case sanMoves of
                Just (_:moves) -> returnJson $ object
                    ["puzzleId" .= puzzleId,
                     "fen" .= fen,
                     "firstMove" .= firstMove,
                     "solution" .= moves,
                     "rating" .= rating,
                     "ratingDeviation" .= ratingDev,
                     "popularity" .= pop,
                     "themes" .= themes]
                _ -> notFound


getRandomPuzzleR :: Handler Value
getRandomPuzzleR = do
    -- The only parameters allowed through this handlers are those defined below
    params <- reqGetParams <$> getRequest
    let allowedParams = ["min_rating", "max_rating", "max_deviation", "min_squares", "max_squares", "themes"]
    let invalidParams = filter (\(name, _) -> name `notElem` allowedParams) params
    unless (null invalidParams) $ sendResponseStatus badRequest400 ("Invalid parameters" :: Text)

    -- Get any parameters we can, otherwise default to left values
    minRating    <- fromMaybe 0    <$> runInputGet (iopt intField "min_rating")
    maxRating    <- fromMaybe 4000 <$> runInputGet (iopt intField "max_rating")
    maxDeviation <- fromMaybe 200  <$> runInputGet (iopt intField "max_deviation")
    minSquares   <- fromMaybe 3    <$> runInputGet (iopt intField "min_squares")
    maxSquares   <- fromMaybe 7    <$> runInputGet (iopt intField "max_squares")
    themes       <- fromMaybe ""   <$> runInputGet (iopt textField "themes")

    -- 400 incorrect requests
    when (minRating > maxRating) $
        sendResponseStatus badRequest400 ("Received higher minimum value than expected: " 
            <> tshow minRating <> " > " <> tshow maxRating <> "" :: Text)
    
    when (minSquares > maxSquares) $
        sendResponseStatus badRequest400 ("Received higher minimum squares than expected: " 
            <> tshow minSquares <> " > " <> tshow maxSquares <> "" :: Text)

    -- Query for theme IDs, and if any aren't found send 400
    themeIds <- runDB $ do
        E.select $ do
            t <- E.from $ E.table @Theme
            E.where_ $ t ^. ThemeName `E.in_` E.valList (words themes)
            return (t ^. ThemeId)

    when (length (words themes) /= length themeIds) $ do
        sendResponseStatus badRequest400 ("One or more themes not found" :: Text)

    r <- liftIO (randomRIO (0.0, 1.0) :: IO Double)

    -- Queries for puzzle meeting criteria under existing Puzzle model.

    -- Themes are further normalized, and the query checks for theme intersections.
    -- Therefore if the theme-list is non-empty, it'll group by puzzle and ensure
    -- requested themes are all present by checking the count of matched themes.

    -- Query is ordered by a randomized float and compared against randomVal field. 
    -- If the query isn't successful, we wrap around to find the lowest randomVal.
    let query orderExpr orderOp = E.selectOne $ do
            puzzle <- E.from $ E.table @Puzzle
            E.where_ $ puzzle ^. PuzzleRating .>=. E.val minRating
            E.where_ $ puzzle ^. PuzzleRating .<=. E.val maxRating
            E.where_ $ puzzle ^. PuzzleRatingDeviation .<=. E.val maxDeviation
            E.where_ $ puzzle ^. PuzzleUciSolutionSquares .>=. E.val minSquares
            E.where_ $ puzzle ^. PuzzleUciSolutionSquares .<=. E.val maxSquares
            E.where_ $ puzzle ^. PuzzleRandomVal `orderOp` E.val r
            E.orderBy [orderExpr (puzzle ^. PuzzleRandomVal)]

            unless (null themes) $ E.where_ $ E.exists $ do
                puzzleTheme <- E.from $ E.table @PuzzleTheme
                E.where_ $ (puzzleTheme ^. PuzzleThemePuzzleId) .==. (puzzle ^. PuzzleId)
                E.where_ $ puzzleTheme ^. PuzzleThemeThemeId `E.in_` E.valList (E.unValue <$> themeIds)
                E.groupBy (puzzleTheme ^. PuzzleThemePuzzleId)
                E.having ((E.count (puzzleTheme ^. PuzzleThemePuzzleId) :: E.SqlExpr (E.Value Int)) .==. E.val (fromIntegral $ length themeIds))
                return ()

            E.limit 1
            return puzzle

    mpuzzle <- runDB $ query E.asc (.>=.)

    case mpuzzle of
        Just (Entity (PuzzleKey puzzleId) _) -> getPuzzleIdR puzzleId
        Nothing -> do
            -- Wrap around (in case r was too high)
            mpuzzle' <- runDB $ query E.desc (.<=.)
            case mpuzzle' of
                Nothing -> notFound
                Just (Entity (PuzzleKey puzzleId) _) -> getPuzzleIdR puzzleId

    where (.>=.) :: (E.PersistField a) => E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value Bool)
          (.>=.) = (E.>=.)
          (.<=.) :: (E.PersistField a) => E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value Bool)
          (.<=.) = (E.<=.)
          (.==.) :: (E.PersistField a) => E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value Bool)
          (.==.) = (E.==.)

