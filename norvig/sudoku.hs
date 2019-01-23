module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad

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

access :: Square -> Map Square a -> a
access s g = fromJust (Map.lookup s g)

peers :: Map Square [Square]
peers = Map.fromList [(s, set (access s units)) | s <- squares]
    where
        set = nub . concat

regular :: String -> Bool
regular grid = length (filter (`elem` "0.123456789") grid) == 81

emptyGrid :: Grid
emptyGrid = Map.fromList [(s, digits) | s <- squares]

parse_grid :: String -> Maybe Grid
parse_grid grid
    | regular grid = foldM assign emptyGrid (zip squares grid)
    | otherwise    = Nothing

assign :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s,d) = foldM (eliminate s) g (filter (/= d) (access s g))

eliminate :: Square -> Grid -> Digit -> Maybe Grid
eliminate = undefined
{-eliminate s g d
    | not elem d (access s g)                  = Just g
    | length (filter (/= d) (access s g)) == 0 = Nothing
    | otherwise =-}


main :: IO ()
main = return ()