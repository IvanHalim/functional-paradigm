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
units = Map.fromList [(s, [delete s u | u <- unitlist, elem s u]) | s <- squares]

access :: Square -> Map Square a -> a
access s g = fromJust (Map.lookup s g)

peers :: Map Square [Square]
peers = Map.fromList [(s, set (access s units)) | s <- squares]
    where
        set = nub . concat

emptyGrid :: Grid
emptyGrid = Map.fromList [(s, digits) | s <- squares]

regular :: String -> Bool
regular grid = length (filter (`elem` "0.123456789") grid) == 81

parse_grid :: String -> Maybe Grid
parse_grid grid
    | regular grid = foldM assign emptyGrid (zip squares grid)
    | otherwise    = Nothing

assign :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s,d)
    | elem d digits = foldM eliminate g (zip (repeat s) other_values)
    | otherwise     = Just g
    where
        other_values = delete d (access s g)

{-eliminate :: Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s,d) =
    let cell = access s g
    in if d `notElem` cell
        then return g
        else do
            let newCell = delete d cell
                newGrid = Map.insert s newCell g
            newGrid2 <- case newCell of
                []      -> Nothing
                [d']    -> let peersOfS = access s peers
                           in foldM eliminate newGrid (zip peersOfS (repeat d'))
                _       -> return newGrid
            foldM (locate d) newGrid2 (access s units)-}

eliminate :: Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s,d)
    | d `notElem` cell = Just g
    | otherwise        = case newGrid2 of
        Just x  -> foldM (locate d) x (access s units)
        Nothing -> Nothing
    where
        cell     = access s g
        newGrid2 = case newCell of
            []   -> Nothing
            [d'] -> foldM eliminate newGrid (zip peersOfS (repeat d'))
            _    -> Just newGrid
            where
                newCell  = delete d cell
                newGrid  = Map.insert s newCell g
                peersOfS = access s peers

locate :: Digit -> Grid -> Unit -> Maybe Grid
locate d g u = case filter ((d `elem`) . (`access` g)) u of
                    []  -> Nothing
                    [s] -> assign g (s,d)
                    _   -> return g


main :: IO ()
main = return ()