module Palindrome where

import Control.Monad
import System.Exit (exitSuccess)
import qualified Data.Char as Char

main :: IO () 
main = forever $ do
  line <- getLine
  let line1 = map Char.toLower line
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!" 
    False -> do
      putStrLn "Nope!"
      exitSuccess