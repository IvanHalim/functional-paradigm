module Basics where

import Prelude hiding (length,sum,product,map,foldr)

----------------------------
-- Higher-Order Functions --
----------------------------

-- | Map a function over the elements in a list.
map :: (a -> b) -> [a] -> [b]
map f []    = []
map f (h:t) = f h : map f t

-- | Reimplement doubleAll using map.
doubleAll' :: [Int] -> [Int]
doubleAll' = map (2*)

-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
notAll' = map not

-- | Reimplement evenAll using map.
evenAll' :: [Int] -> [Bool]
evenAll' = map even

-- | Fold an accumulator function over the elements in a list.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []    = b
foldr f b (h:t) = f h (foldr f b t)

-- | Reimplement sum using foldr.
sum' :: [Int] -> Int
sum' = foldr (+) 0

-- | Reimplement product using foldr.
product' :: [Int] -> Int
product' = foldr (*) 1

-- | Reimplement length using foldr.
length' :: [a] -> Int
length' = foldr (\_ l -> 1 + l) 0

-- | Reimplement allOdd using foldr.
allOdd' :: [Int] -> Bool
allOdd' = foldr (\h r -> odd h && r) True

-- | Use foldr to count the True values in a list of Bools.
countTrues :: [Bool] -> Int
countTrues = foldr (\h r -> if h then 1 + r else r) 0