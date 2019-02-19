{- Exercises: Scope pg.110 -}
-- 1. yes
-- 2. no
-- 3. no, `d` is not defined in `r = d / 2`
-- 4. yes



{- Exercises: Syntax errors pg.115 -}
-- 1. Won't compile. `++` is missing parenthesys `(++)`
-- 2. Won't compile. Strings must use double quotes `"<3"`
-- 3. Will compile ok.



{- Exercises: Chapter exercises pg.122 -}
-- 1.
-- a) Ok
pg122_a = concat [[1,2,3], [4,5,6]]
-- b) Incorrect. ++ is missing parens `(++)`
pg122_b = (++) [1,2,3] [4,5,6]
-- c) Ok
pg122_c = (++) "Hello" "World"
-- d) Ok
pg122_d = ["Hello" ++ "World"]
-- e) Incorrect. Order of arguments should be the the opposite.
pg122_e = "hello" !! 4
-- f) Ok
pg122_f = (!!) "hello" 4
-- g) Ok. Incorrect. First argument must be Int
pg122_g = take 4 "lovely"
-- h) Ok
pg122_h = take 3 "awesome"



{- Exercises: Chapter exercises pg.123 -}
-- 2.
-- a. result is d.
-- b. result is c.
-- c. result is e.
-- d. result is a.
-- e. result is b.



{- Exercises: Chapter exercises pg.124 -}
-- 1.
-- a)
pg124_1a = ("Curry is awesome" ++ "!") == "Curry is awesome!"
pg124_1b = ("Curry is awesome!" !! 4) == 'y'
pg124_1c = (drop 9 "Curry is awesome!") == "awesome!"



{- Exercises: Chapter exercises pg.125 -}
-- 2.
appendStr str1 str2 = str1 ++ str2
getCharAt str position = str !! position
getTheRest str fromPosition = drop fromPosition str

-- 3.
thirdLetter :: String -> Char
thirdLetter str = str !! 2



{- Exercises: Chapter exercises pg.126 -}
-- 4.
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x
