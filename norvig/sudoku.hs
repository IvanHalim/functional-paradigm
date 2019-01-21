module Main where

import Data.List
import Data.Array

type Square = (Char, Char)
type Unit   = [Square]

cross :: String -> String -> [Square]
cross rows cols = [(r,c) | r <- rows, c <- cols]

digits  = "123456789"
rows    = "ABCDEFGHI"
cols    = digits
box     = (('A','1'), ('I','9'))

squares :: [Square]
squares = cross rows cols

unitlist :: [Unit]
unitlist = [cross rows [c]  | c  <- cols] ++
           [cross [r]  cols | r  <- rows] ++
           [cross rs   cs   | rs <- ["ABC","DEF","GHI"],
                              cs <- ["123","456","789"]]

units :: Array Square [Unit]
units = array box [(s, [filter (/= s) u | u <- unitlist, elem s u]) | s <- squares]

peers :: Array Square [Square]
peers = array box [(s, set(units!s)) | s <- squares]
    where set = nub . concat

main :: IO ()
main = return ()