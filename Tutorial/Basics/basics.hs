module Basics where

import Prelude hiding (length,sum,product,map,foldr)

---------------------
-- Basic Functions --
---------------------

-- | Add an integer to itself.
double :: Int -> Int
double x = x + x

-- | Is this integer zero?
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False
-- isZero x = x == 0

-- | Is this integer non-zero?
isNonZero :: Int -> Bool
isNonZero = not . isZero

-- Option 3:
-- isNonZero x = not (isZero x)

-- Option 2:
-- isNonZero 0 = False
-- isNonZero _ = True

-- Option 1:
-- isNonZero x = x /= 0


-- | Computes the average of two floating point numbers.
avg :: Float -> Float -> Float
avg x y = (x + y) / 2.0

-- | Uses avg to compute half of a floating point number.
half :: Float -> Float
half = avg 0

-- Option 2:
-- half = \x -> avg 0 x

-- Option 1:
-- half x = avg 0 x

----------------------
-- Basic Data Types --
----------------------

-- | An example data type with two cases.
data Result = OK Int | Error
  deriving (Eq,Show)

-- | Safely divide two integers.
safeDiv :: Int -> Int -> Result
safeDiv _ 0 = Error
safeDiv x y = OK (x `div` y)

-- | Get the integer from an OK result, or return 0 on an Error.
fromResult :: Result -> Int
-- fromResult (OK i) = i
-- fromResult Error  = 0
fromResult r = case r of
                 Error -> 0
                 OK i  -> i

-- | Add two results.
addResults :: Result -> Result -> Result
addResults (OK i) (OK j) = OK (i + j)
addResults _      _      = Error


-- The definition of Bool in the Haskell Prelude looks like this:
--
--   data Bool = False | True


-- The type Result is similar to the Maybe type in the Prelude:
--
--   data Maybe a = Just a | Nothing