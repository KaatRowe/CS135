module Examples where

----------------------------------------------------------------
mult :: Integer -> Integer -> Integer
mult x y = x * y

multby2 :: Integer -> Integer
multby2 = mult 2

-- How this works multby2 is saying we take the first parameter from input and the second input of mult = 2

-- 2 -> |---------|
--      |  mult   | -> 2 * y
-- y -> |---------|
----------------------------------------------------------------

lessThan2 :: Integer -> Bool 
lessThan2 = (<2)

----------------------------------------------------------------
f :: Integer -> Integer -> Integer -> Integer
f x y z = x + y + z

----------------------------------------------------------------
--lambda function is a function without a name 
-- \ is short for lambda
-- [0..10] is divisible by 3 
    -- filter (\x -> x `mod` 3 == 0) [0..10]


--QUESTIONS
--Hashmap in Haskell? - Techinically yes
--Using higher-order functions can you do a map (f . g) x - Yes and you can do Lambda as well


isEven :: Integral a => a -> Bool
isEven n = n `mod` 2 == 0 

foldr (+) 0 [1, 2, 3]
