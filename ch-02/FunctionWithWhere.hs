module FunctionWithWhere where

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = 
  let
    plusTwo = n + 2
  in 
    print plusTwo

fn1 = x * 3 + y
  where
    x = 3
    y = 1000

fn2 = x * 5
  where
    y = 10
    x = 10 * 5 + y

fn3 = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10
