module Spec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Data.Bool as Bool
import Hangman as H

-- Generators
genWord :: Gen String
genWord = listOf1 $ choose ('a', 'z')

genWordChar :: String -> Gen Char
genWordChar s = elements s

genNotInWordChar :: String -> Gen Char
genNotInWordChar word 
  = elements (filter allowedChar ['a'..'z'])
  where 
    allowedChar azChar = all (\wordChar -> wordChar /= azChar) word
--

-- data Puzzle = Puzzle word   filledInSoFar guessed
-- data Puzzle = Puzzle String [Maybe Char]  [Char]
getGuessed :: Puzzle -> String
getGuessed (Puzzle _ _ guessed) = guessed

getFilledInSoFar :: Puzzle -> [Maybe Char]
getFilledInSoFar (Puzzle _ filledInSoFar _) = filledInSoFar

-- 👇 Feels like logic duplication

calcfilledInSoFar :: String -> Char -> [Maybe Char]
calcfilledInSoFar word ch = map (\ch' -> Bool.bool Nothing (Just ch) (ch == ch')) word

--

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "should fill the guessed character when character is part of the word" $ do
      property $ 
        forAll genWord $ \word -> 
        forAll (genWordChar word) $ \ch ->
          let
            puzzle = H.freshPuzzle word
            filledPuzzle = H.fillInCharacter puzzle ch
          in
            getGuessed filledPuzzle == [ch] 
            && getFilledInSoFar filledPuzzle == calcfilledInSoFar word ch 
    it "should not fill the guessed character when character is not part of the word" $ do
      property $ 
        forAll genWord $ \word -> 
        forAll (genNotInWordChar word) $ \ch ->
          let
            puzzle = H.freshPuzzle word
            filledPuzzle = H.fillInCharacter puzzle ch
          in
            getGuessed filledPuzzle == [ch]
            && getFilledInSoFar filledPuzzle == replicate (length word) Nothing


handleGuess_prop :: String -> Char -> Property
handleGuess_prop word ch = 
  let
    puzzle = H.freshPuzzle word
  in
    monadicIO $ do
    puzzle2 <- run $ handleGuess puzzle ch
    puzzle2' <- run $ handleGuess puzzle ch
    assert $ puzzle2 == puzzle2'

main2 :: IO ()
main2 = verboseCheck $ 
  forAll genWord $ \word -> 
  forAll (genWordChar word) $ \ch ->
    handleGuess_prop word ch