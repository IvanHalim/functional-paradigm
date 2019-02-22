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
regular grid = length (filter (`elem` validDigit) grid) == 81
    where
        validDigit = "0." ++ digits

parseGrid :: String -> Maybe Grid
parseGrid grid
    | regular grid = foldM assign emptyGrid (zip squares grid)
    | otherwise    = Nothing

assign :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s, d)
    | elem d digits = foldM eliminate g (zip (repeat s) otherValues)
    | otherwise     = Just g
    where
        otherValues = delete d (access s g)

eliminate :: Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s, d)
    | d `notElem` cell = Just g
    | otherwise        = case newGrid2 of
        Just g' -> foldM (locate d) g' (access s units)
        Nothing -> Nothing
    where
        cell     = access s g
        newCell  = delete d cell
        newGrid  = Map.insert s newCell g
        peersOfS = access s peers
        newGrid2 = case newCell of
            []   -> Nothing
            [d'] -> foldM eliminate newGrid (zip peersOfS (repeat d'))
            _    -> Just newGrid

locate :: Digit -> Grid -> Unit -> Maybe Grid
locate d g u =
    case filter ((d `elem`) . (`access` g)) u of
        []  -> Nothing
        [s] -> assign g (s, d)
        _   -> Just g

some :: Maybe a -> Maybe a -> Maybe a
some Nothing  Nothing  = Nothing
some Nothing  (Just x) = Just x
some (Just x) _        = Just x

allSizeOne :: Grid -> Bool
allSizeOne g = all (\xs -> length xs == 1) [access s g | s <- squares]

search :: Maybe Grid -> Maybe Grid
search Nothing  = Nothing
search (Just g)
    | allSizeOne g = Just g
    | otherwise    = foldl' some Nothing [search (assign g (s, d)) | d <- access s g]
    where
        (_, s) = minimum [(length (access s g), s) | s <- squares, length (access s g) > 1]

solve :: String -> Maybe Grid
solve grid = search (parseGrid grid)

sublist :: Int -> [a] -> [[a]]
sublist n [] = []
sublist n xs = take n xs : sublist n (drop n xs)

centerString :: Int -> String -> String
centerString n s = lead ++ s ++ trail
    where
        l = (n - length s) `div` 2
        t = n - length s - l
        lead  = replicate l ' '
        trail = replicate t ' '

gridToString :: Maybe Grid -> String
gridToString Nothing  = ""
gridToString (Just g) = unlines l5
    where
        l1 = map snd (Map.toList g)
        l2 = map (centerString width) l1
        l3 = map concat (sublist 3 l2)
        l4 = map (intercalate "|") (sublist 3 l3)
        l5 = intercalate [line] (sublist 3 l4)
        width   = 1 + maximum [length (access s g) | s <- squares]
        hyphens = replicate (width * 3) '-'
        line    = intercalate "+" (replicate 3 hyphens)

display :: Maybe Grid -> IO ()
display = putStrLn . gridToString

grid1  = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
grid2  = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
hard1  = ".....6....59.....82....8....45........3........6..3.54...325..6.................."

main :: IO ()
main = return ()