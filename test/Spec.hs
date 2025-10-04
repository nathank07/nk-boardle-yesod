import Test.Hspec
import Boardle.Boardle
    ( checkValidityOfGame,
      getGuesses,
      Guess(..)
    )

main :: IO ()
main = hspec spec

startFEN :: String
startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

guess :: String -> [Guess]
guess str = map Unknown (answer str)

answer :: String -> [String]
answer = map (:[])

spec :: Spec
spec = do
    describe "Game validity" $ do
        it "Returns true for valid games" $ do
            checkValidityOfGame startFEN ["e4", "d5", "exd5", "e5", "dxe6"] `shouldBe` True
        it "Returns true for empty move list" $ do
            checkValidityOfGame startFEN [] `shouldBe` True
        it "Returns false for invalid games" $ do
            checkValidityOfGame startFEN ["e4", "e5", "e5"] `shouldBe` False
    describe "Wordle Guesses" $ do
        it "Correctly identifies correct all letters (answer panda, guessing panda)" $ do
            getGuesses (guess "panda") (answer "panda") `shouldBe` Just [Green, Green, Green, Green, Green]
        it "Correctly identifies one yellow letter (answer crisp, guessing panda)" $ do
            getGuesses (guess "panda") (answer "crisp") `shouldBe` Just [Yellow, Gray, Gray, Gray, Gray]
        it "Correctly identifies puts one green when there's multiple yellows (answer ccccc, guessing crane)" $ do
            getGuesses (guess "crane") (answer "ccccc") `shouldBe` Just [Green, Gray, Gray, Gray, Gray]
        it "Correctly prioritizes greens over yellows (answer cxxcx, guessing yyycy)" $ do
            getGuesses (guess "yyycy") (answer "cxxcx") `shouldBe` Just [Gray, Gray, Gray, Green, Gray]
        it "Correctly identifies two yellows (answer axnax, guessing panda)" $ do 
            getGuesses (guess "panda") (answer "axnax") `shouldBe` Just [Gray, Yellow, Green, Gray, Yellow]
        it "Correctly identifies two yellows (answer elder, guessing lever)" $ do 
            getGuesses (guess "lever") (answer "elder") `shouldBe` Just [Yellow, Yellow, Gray, Green, Green]
        it "Fails when it's not all unknowns" $ do
            getGuesses (Green : guess "xyzdc") (answer "acrisp") `shouldBe` Nothing
            getGuesses (guess "xyzdc" ++ [Gray]) (answer "acrisp") `shouldBe` Nothing
    --describe "Moves conversion" $ do