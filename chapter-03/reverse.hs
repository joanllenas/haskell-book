module Reverse where

{- Exercises: Chapter exercises pg.126 -}

-- 5.
rvrs :: String -> String
rvrs str = let
    curry = take 5 str
    is = drop 6 (take 8 str)
    awesome = drop 9 str
  in
    awesome ++ " " ++ is ++ " " ++ curry

main :: IO()
-- main = print (rvrs "Curry is awesome")
main = print $ rvrs "Curry is awesome"
