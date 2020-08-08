module Main where

import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import Data.Char (toLower)
import Hangman as H

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  word <- H.randomWord'
  let puzzle = H.freshPuzzle (fmap toLower word)
  H.runGame puzzle
