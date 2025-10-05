module Boardle.Boardle
    ( getSANMoves
    , getSANMoves'
    , getGuesses
    , checkValidityOfGame
    , Guess(Green, Yellow, Gray, Unknown)
    , FEN
    , SAN
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

isUnknown :: Guess -> Bool
isUnknown (Unknown _) = True
isUnknown _ = False

type FEN = String
type UCI = String
type SAN = String
type Answer = String

getGuesses :: [Guess] -> [Answer] -> Maybe [Guess]
getGuesses guesses answers = 
    if any (not . isUnknown) guesses || length guesses /= length answers
        then Nothing
        else Just $ getGuesses' (getGuesses'' guesses answers) answers
    where 
        getGuesses' = zipWith f
            where 
                f (Unknown a) b 
                    | a == b = Green
                    | otherwise = Gray
                f (MaybeYellow a) b 
                    | a == b = Green
                    | otherwise = Yellow
                f _ _ = Gray 
        getGuesses'' = foldl replaceFirstYellow 
            where 
                replaceFirstYellow [] _ = []
                replaceFirstYellow ((Unknown g):gs) a 
                    | a == g = MaybeYellow g : gs
                    | otherwise = (Unknown g) : replaceFirstYellow gs a
                replaceFirstYellow (g:gs) a = g : replaceFirstYellow gs a


getSANMoves :: Position -> [UCI] -> Maybe [SAN]
getSANMoves pos uciMoves = fmap (reverse . snd) $ foldM step (pos, []) uciMoves
    where
        step (currPos, sanMoves) uci =
            (\p -> (unsafeDoPly currPos p, toSAN currPos p : sanMoves))
                <$> fromUCI currPos uci

getSANMoves' :: FEN -> [UCI] -> Maybe [SAN]
getSANMoves' fenStr uciMoves = (fromFEN fenStr) >>= (\pos -> getSANMoves pos uciMoves)

getUCIMove :: FEN -> SAN -> Maybe UCI
getUCIMove fenStr sanMove = (fromFEN fenStr) >>= (\pos -> getUCIMove' pos sanMove) 

getUCIMove' :: Position -> SAN -> Maybe UCI
getUCIMove' pos sanMove = 
    case fromSAN pos sanMove of
        Left _ -> Nothing
        Right x -> Just $ toUCI x

checkValidityOfGame :: FEN -> [SAN] -> Bool
checkValidityOfGame fenStr sanMoves = 
    case (fromFEN fenStr) of 
        Nothing -> False
        Just pos -> isPos $ foldM step pos sanMoves
    where 
        step currPos san = do
            uci <- getUCIMove' currPos san
            ply <- fromUCI currPos uci
            return (unsafeDoPly currPos ply)
        isPos (Just _) = True
        isPos Nothing = False

