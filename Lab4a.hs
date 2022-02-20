-- Exercise set 4a:
--
-- * using type classes
-- * working with lists
--
-- Type classes you'll need
--  * Eq
--  * Ord
--  * Num
--  * Fractional
--
-- Useful functions:
--  * maximum
--  * minimum
--  * sort

module Lab4a where

import Data.List
import Data.Ord

--Help from Colin, Khadija

------------------------------------------------------------------------------
-- Ex 1: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--   allEqual [] ==> True
--   allEqual [1,2,3] ==> False
--   allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:y:zs) = x == y && allEqual (y:zs)


------------------------------------------------------------------------------
-- Ex 2: implement the function distinct which returns True if all
-- values in a list are different.
--
-- Hint: a certain function from the lecture material can make this
-- really easy for you.
--
-- Examples:
--   distinct [] ==> True
--   distinct [1,1,2] ==> False
--   distinct [1,2] ==> True

distinct :: Eq a => [a] -> Bool
distinct [] = False
distinct [x] = True
distinct (x:y:zs) = (x /= y) && distinct (y:zs)
------------------------------------------------------------------------------
-- Ex 3: implement the function middle that returns the middle value
-- (not the smallest or the largest) out of its three arguments.
--
-- The function should work on all types in the Ord class. Give it a
-- suitable type signature.
--
-- Examples:
--   middle 'b' 'a' 'c'  ==> 'b'
--   middle 1 7 3        ==> 3

middle :: Ord a => a -> a -> a -> a
middle x y z 
    | x <= y && y <= x  = y
    | y <= x && x <= z = x
    | x <= z && z <= y = z

------------------------------------------------------------------------------
-- Ex 4: return the range of an input list, that is, the difference
-- between the smallest and the largest element.
--
-- Your function should work on all suitable types, like Float and
-- Int. You'll need to add _class constraints_ to the type of range.
--
-- It's fine if your function doesn't work for empty inputs.
--
-- Examples:
--   rangeOf [4,2,1,3]          ==> 3
--   rangeOf [1.5,1.0,1.1,1.2]  ==> 0.5


rangeOf :: (Num p, Ord a) => [a] -> p
rangeOf [] = 0
rangeOf [x] = 1
rangeOf (x:y:xs) = rangeMax - rangeMin where
    rangeMax = if x > y then rangeOf (x:xs) else rangeOf (y:xs)
    rangeMin = if x < y then rangeOf (x:xs) else rangeOf (y:xs)

------------------------------------------------------------------------------
-- Ex 5: given a list of lists, return the longest list. If there
-- are multiple lists of the same length, return the list that has
-- the smallest _first element_.
--
-- (If multiple lists have the same length and same first element,
-- you can return any one of them.)
--
-- Give the longest function a suitable type.
--
-- Examples:
--   longest [[1,2,3],[4,5],[6]] ==> [1,2,3]
--   longest ["bcd","def","ab"] ==> "bcd"

--longest [] = 0
--longest (x:y:xs) 
  --  | length x > length y && longest (x:xs) = x
    -- | length x < length y  && longest (y:xs) = y
    -- | otherwise = if head x < head y then y else x

------------------------------------------------------------------------------
-- Ex 6: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
--
-- You'll need to add _class constraints_ to the type of incrementKey
-- to make the function work!
--
-- The function needs to be generic and handle all compatible types,
-- see the examples.
--
-- Examples:
--   incrementKey True [(True,1),(False,3),(True,4)] ==> [(True,2),(False,3),(True,5)]
--   incrementKey 'a' [('a',3.4)] ==> [('a',4.4)]

--incrementKey :: k -> [(k,v)] -> [(k,v)]
--incrementKey k [(i, j)] = map (incrementKey' (i, j) k) [(i, j)]

--incrementKey' :: (a, b) -> a -> (a, b)
--incrementKey' (i, j) k = if k == i then j + 1 else (i, j)
------------------------------------------------------------------------------
-- Ex 7: compute the average of a list of values of the Fractional
-- class.
--
-- There is no need to handle the empty list case.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional


average :: (Fractional a, Foldable t) => t a -> a
average xs = sum xs / fromIntegral (length xs)
