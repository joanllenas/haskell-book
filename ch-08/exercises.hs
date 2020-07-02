fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci nth 
  = (fibonacci (nth-1)) 
  + (fibonacci (nth-2))


-- dividedBy :: Integral a => a -> a -> (a, a)
-- dividedBy num denom 
--   = go num denom 0
--   where 
--     go n d count
--       | n < d = (count, n)
--       | otherwise = go (n - d) d (count + 1)

-- dividedBy fixed
data DividedResult 
  = Result Integer
  | DividedByZero deriving Show

signFn :: (Ord a, Num a, Num b) => a -> b -> b
signFn n = if n < 0 then (* (-1)) else (*1)

go n d count
  | n < d = (count, n)
  | otherwise = go (n - d) d (count + 1)

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy num denom
  | denom /= 0 = 
    let 
      res = fst $ go (abs num) (abs denom) 0
    in 
      Result (signFn num . signFn denom $ res)
  | otherwise = DividedByZero


mc91 :: (Ord a, Num a) => a -> a
mc91 n
  | n > 100    = n - 10
  | otherwise  = mc91 $ mc91 $ n + 11

