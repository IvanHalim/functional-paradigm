module Basics where

import Prelude hiding (length,sum,product,map,foldr)

---------------
-- Recursion --
---------------

-- | An example of a recursive data type.
data List
   = Nil
   | Cons Int List
  deriving (Eq,Show)

-- | The empty list.
empty :: List
empty = Nil

-- | The list: [2,3,4]
exList :: List
exList = Cons 2 (Cons 3 (Cons 4 Nil))

-- | Compute the length of a list.
listLength :: List -> Int
listLength Nil        = 0
listLength (Cons _ t) = 1 + listLength t

-- | Compute the sum of the integers in a list.
listSum :: List -> Int
listSum Nil        = 0
listSum (Cons h t) = h + listSum t


-------------------
-- Haskell Lists --
-------------------

-- Haskell's built-in list and string types
--
-- data [a]
--    = []         -- Nil
--    | a : [a]    -- Cons

-- The definition of String in the Haskell Prelude looks like this:
--
--   type String = [Char]


-- | Compute the length of a list.
length :: [a] -> Int
length []    = 0
length (_:t) = 1 + length t

-- | Compute the sum of an integer list.
sum :: [Int] -> Int
sum []    = 0
sum (h:t) = h + sum t

-- | Compute the product of the elements in a list.
product :: [Int] -> Int
product []    = 1
product (h:t) = h * product t

allOdd :: [Int] -> Bool
allOdd []    = True
allOdd (h:t) = odd h && allOdd t

-- | Double all the elements in an integer list.
doubleAll :: [Int] -> [Int]
doubleAll []    = []
doubleAll (h:t) = (2 * h) : doubleAll t

-- | Flip all of the boolean values in a boolean list.
notAll :: [Bool] -> [Bool]
notAll []    = []
notAll (h:t) = not h : notAll t

-- | Apply the even function to all elements in the list.
evenAll :: [Int] -> [Bool]
evenAll []    = []
evenAll (h:t) = even h : evenAll t

-- Use a *type parameter* to define lists that contain elements of any type:
--
--    data List a
--       = Nil
--       | Cons a (List a)   -- all elements must be of the *same* type
--
--    listLength :: List a -> Int
--    listSum    :: List Int -> Int
--    listSum'   :: Num a => List a -> a