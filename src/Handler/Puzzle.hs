{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Puzzle where

import Import

-- getPuzzleIdR :: Text -> Handler Value
-- getPuzzleIdR puzzleId = do
--     mpuzzle <- liftIO $ getFullPuzzleById (PuzzleID $ Import.unpack puzzleId)
--     maybe notFound returnJson mpuzzle


-- getRandomPuzzleR :: Handler Value
-- getRandomPuzzleR = do
--     params <- reqGetParams <$> getRequest
--     let allowedParams = ["low_rating", "high_rating", "max_deviation", "min_squares", "max_squares"]
--     let invalidParams = filter (\(name, _) -> name `notElem` allowedParams) params
--     unless (null invalidParams) $ sendResponseStatus badRequest400 ("Invalid parameters" :: Text)
    
--     lowRating    <- fromMaybe 0    <$> runInputGet (iopt intField "low_rating")
--     highRating   <- fromMaybe 4000 <$> runInputGet (iopt intField "high_rating")
--     maxDeviation <- fromMaybe 200  <$> runInputGet (iopt intField "max_deviation")
--     minSquares   <- fromMaybe 3    <$> runInputGet (iopt intField "min_squares")
--     maxSquares   <- fromMaybe 7    <$> runInputGet (iopt intField "max_squares")

--     mpuzzleId <- liftIO $ getRandomPuzzleID
--         (PuzzleSearch
--             { psMinRating = lowRating
--             , psMaxRating = highRating
--             , psMaxRatingDeviation = maxDeviation
--             , psMinSquares = minSquares
--             , psMaxSquares = maxSquares
--             , psThemes = []
--             })
--     case mpuzzleId of
--         Nothing -> notFound
--         Just pid -> do
--             mpuzzle <- liftIO $ getFullPuzzleById pid
--             maybe notFound returnJson mpuzzle