module PoemLines where
import Data.Char


firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual 
  = [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" 
  ]

lTrimNewline :: String -> String
lTrimNewline [] = ""
lTrimNewline ('\n':xs) = lTrimNewline xs
lTrimNewline (x:xs) = x : xs

myLines :: String -> [String]
myLines "" = []
myLines s = 
  let
    str = takeWhile (\ch -> ch /= '\n') s
    rest = dropWhile (\ch -> ch /= '\n') s
  in
    [str] ++ myLines (lTrimNewline rest)


main :: IO () 
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)
