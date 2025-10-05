module Boardle.Boardle
    ( getSANMoves
    , getSANMoves'
    , getGuesses
    , checkValidityOfGame
    , Guess(Green, Yellow, Gray, Unknown)
    , FEN(..)
    , UCI(..)
    , SAN(..)
    , Answer(..)
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

newtype FEN = FEN String deriving       (Eq, Show)
newtype UCI = UCI String deriving       (Eq, Show)
newtype SAN = SAN String deriving       (Eq, Show)
newtype Answer = Answer String deriving (Eq, Show)

getGuesses :: [Guess] -> [Answer] -> Maybe [Guess]
getGuesses guesses answers = 
    if any (not . isUnknown) guesses || length guesses /= length answers
        then Nothing
        else Just $ getGuesses' 
                    (getGuesses'' guesses (map (\(Answer a) -> a) answers)) 
                    (map (\(Answer a) -> a) answers)
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
        step (currPos, sanMoves) (UCI uci) =
            (\p -> (unsafeDoPly currPos p, SAN (toSAN currPos p) : sanMoves))
                <$> fromUCI currPos uci

getSANMoves' :: FEN -> [UCI] -> Maybe [SAN]
getSANMoves' (FEN fenStr) uciMoves = (fromFEN fenStr) >>= (\pos -> getSANMoves pos uciMoves)

getUCIMove :: FEN -> SAN -> Maybe UCI
getUCIMove (FEN fenStr) sanMove = (fromFEN fenStr) >>= (\pos -> getUCIMove' pos sanMove) 

getUCIMove' :: Position -> SAN -> Maybe UCI
getUCIMove' pos (SAN sanMove) = 
    case fromSAN pos sanMove of
        Left _ -> Nothing
        Right x -> Just $ UCI (toUCI x)

checkValidityOfGame :: FEN -> [SAN] -> Bool
checkValidityOfGame (FEN fenStr) sanMoves = 
    case (fromFEN fenStr) of 
        Nothing -> False
        Just pos -> isPos $ foldM step pos sanMoves
    where 
        step currPos san = do
            (UCI uci) <- getUCIMove' currPos san
            ply <- fromUCI currPos uci
            return (unsafeDoPly currPos ply)
        isPos (Just _) = True
        isPos Nothing = False

