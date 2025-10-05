module Boardle.Boardle
    ( getSANMoves
    , getSANMoves'
    , getGuesses
    , getUCIMove
    , checkValidityOfGame
    , Guess(Unknown)
    , GuessResult(GreenResult, YellowResult, GrayResult)
    , FEN(..)
    , UCI(..)
    , SAN(..)
    , Answer(..)
    ) where

import Game.Chess
import Game.Chess.SAN
import Control.Monad (foldM)

data Guess a = Unknown a
    deriving (Eq, Show)

data GuessResult = GreenResult
                 | YellowResult  
                 | GrayResult
    deriving (Eq, Show)

-- Internal helper type for processing
data ProcessGuess a = ProcessGreen
                    | ProcessYellow
                    | ProcessMaybeYellow a
                    | ProcessGray
                    | ProcessUnknown a
    deriving (Eq, Show)


newtype FEN = FEN String deriving       (Eq, Show)
newtype UCI = UCI String deriving       (Eq, Show)
newtype SAN = SAN String deriving       (Eq, Show)
newtype Answer a = Answer a deriving (Eq, Show)

getGuesses :: Eq a => [Guess a] -> [Answer a] -> Maybe [GuessResult]
getGuesses guesses answers = 
    if length guesses /= length answers
        then Nothing
        else Just $ getGuesses' 
                    (getGuesses'' (map toProcessGuess guesses) (map (\(Answer a) -> a) answers)) 
                    (map (\(Answer a) -> a) answers)
    where 
        toProcessGuess (Unknown a) = ProcessUnknown a
        
        getGuesses' = zipWith f
            where 
                f (ProcessUnknown a) b 
                    | a == b = GreenResult
                    | otherwise = GrayResult
                f (ProcessMaybeYellow a) b 
                    | a == b = GreenResult
                    | otherwise = YellowResult
                f _ _ = GrayResult 
        getGuesses'' = foldl replaceFirstYellow 
            where 
                replaceFirstYellow [] _ = []
                replaceFirstYellow ((ProcessUnknown g):gs) a 
                    | a == g = ProcessMaybeYellow g : gs
                    | otherwise = (ProcessUnknown g) : replaceFirstYellow gs a
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

