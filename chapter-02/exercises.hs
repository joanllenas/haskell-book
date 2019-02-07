triple x = 3 * x


{- Comprehension Check -}
-- 1.
-- let half x = x / 2
-- let square x = x * x

-- 2.
circleArea1 radius = 3.14 * (radius * radius)

-- 3.
circleArea2 radius = pi * (radius * radius)



{- Parentheses and Association -}
-- 1.
-- a and b yeld different results.

-- 2.
-- a and b yeld the same result.

-- 3.
-- a and b yeld different results.



{- Exercises: A Head Code p.88 -}

-- 1.
-- let x = 5 in x --> 5
hc1 = x
  where x = 5

-- 2.
-- let x = 5 in x * x --> 25
hc2 = x * x
  where x = 5

-- 3.
-- let x = 5; y = 6 in x * y --> 30
hc3 = x * y
  where 
    x = 5
    y = 6

-- 4.
-- let x = 3; y = 1000 in x + 3 --> 6
hc4 = x + 3
    where
      x = 3
      y = 1000



{- Exercises: A Head Code p.89 -}

-- 1.
let_1 = let 
    x = 3 
    y = 1000 
  in 
    x * 3 + y
where_1 = x * 3 + y
  where
    x = 3
    y = 1000

-- 2.
let_2 = let
    y = 10
    x = 10 * 5
  in
    x * 5
where_2 = x * 5
  where
    y = 10
    x = 10 * 5

-- 3.
let_3 = 
  let
    x = 7
    y = negate x
    z = y * 10
  in
    z / x + y

where_3 = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10



{- Exercises: Parenthisation p.90 -}

-- 1.
paren1 = 2 + 2 * 3 - 1 == 2 + (2 * 3) - 1

-- 2.
paren2 = ((^) 10 $ 1 + 1) == 10 ^ (1 + 1)

-- 3.
paren3 = 2 ^ 2 * 4 ^ 5 + 1 == ((2 ^ 2) * (4 ^ 5)) + 1



{- Exercises: Equivalent expressions p.91 -}

-- 1.
equi1 = 1 + 1 == 2 -- True
equi2 = 10 ^ 2 == 10 + 9 * 10 -- True
equi3 = 400 - 37 == ((-) 37 400) -- False
-- equi4 = (100 `div` 3) == (100 / 3) -- False and Error -> `div` is fractional, `/` is integral division
equi5 = 2 * 5 + 18 == 2 * (5 + 18) -- False



{- Exercises: More fun with functions p.92 -}

-- 1.
z = 7
y = z + 8
x = y ^ 2
waxOn = x * 5 -- 1125

waxOn1 = 10 + waxOn == 1135
waxOn2 = (+10) waxOn == 1135
waxOn3 = (-) 15 waxOn == -1110
waxOn4 = (-) waxOn 15 == 1110



{- Exercises: p.93 -}

-- 2 & 3.
ex2_3 = triple waxOn == 1125 * 3

-- 4.
waxOn_where = x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

-- 6.
waxOff x = triple x
