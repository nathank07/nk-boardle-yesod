module Types
    ( Puzzle(..)
    , Move(..)

    ) where

import Data.Text (Text)
import Data.Char (toLower, isUpper)

data Piece = Pawn Color
           | Bishop Color
           | Rook Color
           | King Color
           | Queen Color
           | Knight Color
            deriving (Show, Eq)

mkPiece :: Char -> Maybe Piece
mkPiece c = case toLower c of 
    'p' -> Just $ Pawn color
    'b' -> Just $ Bishop color
    'r' -> Just $ Rook color
    'k' -> Just $ King color
    'q' -> Just $ Queen color
    'n' -> Just $ Knight color
    _ -> Nothing
    where color = if isUpper c then White else Black

data Color = White | Black
    deriving (Show, Eq)
    
data CastlingRights = Long | Short
    deriving (Show, Eq)

data Move = Move
    { piece        :: Piece
    , fromSquare   :: Text
    , toSquare     :: Text
    , isCapture    :: Bool
    , isCheck      :: Bool
    , isCheckmate  :: Bool
    , isPromotion  :: Maybe Piece
    , moveColor    :: Color
    }

data Rating = Rating 
    { elo      :: Int
    , deviation   :: Int
    }

data Puzzle = Puzzle 
    { puzzleID :: Text
    , puzzleBoard :: Text
    , colorToMove :: Color
    , enPassant :: Maybe Text
    , whiteCastling :: [CastlingRights]
    , blackCastling :: [CastlingRights]
    , moves :: [Move]
    , rating :: Rating
    , themes :: [Text]
    }