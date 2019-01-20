module Sudoku where

type Square = (Char, Char)
type Unit   = [Square]

cross :: String -> String -> [Square]
cross rows cols = [(r, c) | r <- rows, c <- cols]

digits  = "123456789"
rows    = "ABCDEFGHI"
cols    = digits

squares :: [Square]
squares = cross rows cols

unitlist :: [Unit]
unitlist = [cross rows [c]  | c  <- cols] ++
           [cross [r]  cols | r  <- rows] ++
           [cross rs   cs   | rs <- ["ABC", "DEF", "GHI"],
                              cs <- ["123", "456", "789"]]