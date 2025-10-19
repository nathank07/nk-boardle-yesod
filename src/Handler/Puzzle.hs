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
    params <- reqGetParams <$> getRequest
    let allowedParams = ["low_rating", "high_rating", "max_deviation", "min_squares", "max_squares", "themes"]
    let invalidParams = filter (\(name, _) -> name `notElem` allowedParams) params
    unless (null invalidParams) $ sendResponseStatus badRequest400 ("Invalid parameters" :: Text)

    lowRating    <- fromMaybe 0    <$> runInputGet (iopt intField "low_rating")
    highRating   <- fromMaybe 4000 <$> runInputGet (iopt intField "high_rating")
    maxDeviation <- fromMaybe 200  <$> runInputGet (iopt intField "max_deviation")
    minSquares   <- fromMaybe 3    <$> runInputGet (iopt intField "min_squares")
    maxSquares   <- fromMaybe 7    <$> runInputGet (iopt intField "max_squares")
    themes       <- fromMaybe ""   <$> runInputGet (iopt textField "themes")
    
    r <- liftIO (randomRIO (0.0, 1.0) :: IO Double)

    themeIds <- runDB $ do
        E.select $ do
            t <- E.from $ E.table @Theme
            E.where_ $ t E.^. ThemeName `E.in_` E.valList (words themes)
            return (t E.^. ThemeId)

    liftIO $ putStrLn ""
    print themeIds
    liftIO $ putStrLn ""
    
    mpuzzle <- runDB $ selectFirst 
        [ PuzzleRating >=. lowRating
        , PuzzleRating <=. highRating  
        , PuzzleRatingDeviation <=. maxDeviation
        , PuzzleUciSolutionSquares >=. minSquares
        , PuzzleUciSolutionSquares <=. maxSquares
        , PuzzleRandomVal >=. r
        ] 
        [Asc PuzzleRandomVal]
    
    -- Wrap around if random value is too high
    mpuzzle' <- case mpuzzle of
        Just _ -> return mpuzzle
        Nothing -> runDB $ selectFirst
            [ PuzzleRating >=. lowRating
            , PuzzleRating <=. highRating
            , PuzzleRatingDeviation <=. maxDeviation  
            , PuzzleUciSolutionSquares >=. minSquares
            , PuzzleUciSolutionSquares <=. maxSquares
            , PuzzleRandomVal <. r
            ]
            [Desc PuzzleRandomVal]
    
    case mpuzzle' of
        Nothing -> notFound
        Just (Entity puzzleKey _) -> do
            let (PuzzleKey puzzleId) = puzzleKey
            getPuzzleIdR puzzleId
