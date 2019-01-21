module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Maybe

type Square = String
type Unit   = [Square]
type Digit  = Char
type Grid   = Map Square [Digit]

cross :: String -> String -> [Square]
cross rows cols = [r:c:[] | r <- rows, c <- cols]

digits  = "123456789"
rows    = "ABCDEFGHI"
cols    = digits

squares :: [Square]
squares = cross rows cols

unitlist :: [Unit]
unitlist = [cross rows [c]  | c  <- cols] ++
           [cross [r]  cols | r  <- rows] ++
           [cross rs   cs   | rs <- ["ABC","DEF","GHI"],
                              cs <- ["123","456","789"]]

units :: Map Square [Unit]
units = Map.fromList [(s, [filter (/= s) u | u <- unitlist, elem s u]) | s <- squares]

set :: Eq a => Maybe [[a]] -> [a]
set Nothing  = []
set (Just x) = nub (concat x)

peers :: Map Square [Square]
peers = Map.fromList [(s, set (Map.lookup s units)) | s <- squares]

emptyGrid :: Grid
emptyGrid = Map.fromList [(s, digits) | s <- squares]

parse_grid :: String -> Maybe Grid
parse_grid = undefined

grid_values :: String -> Maybe ([(Square, Char)])
grid_values grid =
    let chars = [c | c <- grid, elem c "0.123456789"] in
        if length chars /= 81 then Nothing
        else Just (zip squares chars)

main :: IO ()
main = return ()