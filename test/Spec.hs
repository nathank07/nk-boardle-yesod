{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec
import Boardle.Boardle
    ( checkValidityOfGame,
      getGuesses,
      Guess(..),
      GuessResult(..),
      Answer(..),
      FEN(..),
      SAN(..)
    )

main :: IO ()
main = hspec spec

startFEN :: FEN
startFEN = FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

guess :: String -> [Guess String]
guess str = map Unknown (answer' str)

answer :: String -> [Answer String]
answer = map (Answer . (:[])) 

answer' :: String -> [String]
answer' = map (:[])

spec :: Spec
spec = do
    describe "Game validity" $ do
        it "Returns true for valid games" $ do
            checkValidityOfGame startFEN [SAN "e4", SAN "d5", SAN "exd5", SAN "e5", SAN "dxe6"] `shouldBe` True
        it "Returns true for empty move list" $ do
            checkValidityOfGame startFEN [] `shouldBe` True
        it "Returns false for invalid games" $ do
            checkValidityOfGame startFEN [SAN "e4", SAN "e5", SAN "e5"] `shouldBe` False
    describe "Wordle Guesses" $ do
        it "Correctly identifies correct all letters (answer panda, guessing panda)" $ do
            getGuesses (guess "panda") (answer "panda") `shouldBe` Just [GreenResult, GreenResult, GreenResult, GreenResult, GreenResult]
        it "Correctly identifies one yellow letter (answer crisp, guessing panda)" $ do
            getGuesses (guess "panda") (answer "crisp") `shouldBe` Just [YellowResult, GrayResult, GrayResult, GrayResult, GrayResult]
        it "Correctly identifies puts one green when there's multiple yellows (answer ccccc, guessing crane)" $ do
            getGuesses (guess "crane") (answer "ccccc") `shouldBe` Just [GreenResult, GrayResult, GrayResult, GrayResult, GrayResult]
        it "Correctly prioritizes greens over yellows (answer cxxcx, guessing yyycy)" $ do
            getGuesses (guess "yyycy") (answer "cxxcx") `shouldBe` Just [GrayResult, GrayResult, GrayResult, GreenResult, GrayResult]
        it "Correctly identifies two yellows (answer axnax, guessing panda)" $ do 
            getGuesses (guess "panda") (answer "axnax") `shouldBe` Just [GrayResult, YellowResult, GreenResult, GrayResult, YellowResult]
        it "Correctly identifies two yellows (answer elder, guessing lever)" $ do 
            getGuesses (guess "lever") (answer "elder") `shouldBe` Just [YellowResult, YellowResult, GrayResult, GreenResult, GreenResult]