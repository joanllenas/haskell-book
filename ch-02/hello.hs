module Hello where

sayHello :: String -> IO ()
sayHello x = putStrLn $ "Hello, " ++ x ++ "!"

triple x = x * 3

foo x =
  let 
    y = x * 2
    z = x ^ 2
  in 2 * y * z

waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y ^2

waxOff x = triple x
