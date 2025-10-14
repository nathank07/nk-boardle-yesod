{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Puzzle where

import Import
import Database.GetPuzzle

getPuzzleIdR :: Text -> Handler Value
getPuzzleIdR puzzleId = do
    mpuzzle <- liftIO $ getFullPuzzleById (PuzzleID $ unpack puzzleId)
    maybe notFound returnJson mpuzzle

getRandomPuzzleR :: Handler Value
getRandomPuzzleR = do
    mpuzzleId <- liftIO $ getRandomPuzzleID (PuzzleSearch 1500 1600 100 5 5 [])
    case mpuzzleId of
        Nothing -> notFound
        Just pid -> do
            mpuzzle <- liftIO $ getFullPuzzleById pid
            maybe notFound returnJson mpuzzle