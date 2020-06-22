module Exercises where

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aTob _ a = aTob a