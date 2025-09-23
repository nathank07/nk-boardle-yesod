module Boardle.Boardle
    ( getSANMoves
    , getSANMoves'
    ) where

import Game.Chess
import Game.Chess.SAN
import Control.Monad (foldM)

data Guess = Green
           | Yellow
           | MaybeYellow String
           | Gray
           | Unknown String
    deriving (Eq, Show)

type FEN = String
type UCI = String
type SAN = String
type Answer = String

getGuesses' :: [Guess] -> [Answer] -> [Guess]
getGuesses' = zipWith f
    where 
        f (Unknown a) b 
         | a == b = Green
         | otherwise = Gray
        f (MaybeYellow a) b 
         | a == b = Green
         | otherwise = Yellow
        f _ _ = Gray 

getGuesses'' :: [Guess] -> [Answer] -> [Guess]
getGuesses'' = foldl replaceFirstYellow 
    where 
        replaceFirstYellow [] _ = []
        replaceFirstYellow ((Unknown g):gs) a 
         | a == g = MaybeYellow g : gs
         | otherwise = (Unknown g) : replaceFirstYellow gs a
        replaceFirstYellow (g:gs) a = g : replaceFirstYellow gs a

getGuesses :: [Guess] -> [Answer] -> [Guess]
getGuesses guesses answers = getGuesses' (getGuesses'' guesses answers) answers

getSANMoves :: Position -> [FEN] -> Maybe [SAN]
getSANMoves pos uciMoves = fmap (reverse . snd) $ foldM step (pos, []) uciMoves
    where
        step (currPos, sanMoves) uci =
            (\p -> (unsafeDoPly currPos p, toSAN currPos p : sanMoves))
                <$> fromUCI currPos uci

getSANMoves' :: FEN -> [UCI] -> Maybe [SAN]
getSANMoves' fenStr uciMoves = (fromFEN fenStr) >>= (\pos -> getSANMoves pos uciMoves)

