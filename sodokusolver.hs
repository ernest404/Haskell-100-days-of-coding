import Data.List (transpose)
import Distribution.Simple.Command (OptDescr(BoolOpt))

-- Basic Declarations

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char
-- Grid = [[a]]

-- Example of how this works

easy :: Grid
easy = ["2....1.38", --"2...1" = ['2', '.', '.', 1] list of characters this is a row because it is a list of something
        "........5",
        ""]

empty :: Grid
empty = replicate 9 (replicate 9 '.') -- first replicate creates a list of '.' characters and replicates it 9 times in a list

-- Create a function valid to ascertain that the grid does not have any duplicates in the rows, columns and smaller matrixs

-- row helper function: extracts all the rows from the sodoku matrix as a list of rows.But Matrix is a list of rows
rows :: Matrix a -> [Row a]
rows = id

-- rows.rows = id   identity function: does nothing

-- cols helper function: extracts columns as list of rows.The col gets pulled out as a row.Matrix transposition 

cols :: Matrix a -> [Row a]
cols = transpose

-- cols.cols = id  gets you back to the same point.

-- boxs helper function: extracts boxs as list of rows. A box is store as a row.
boxs :: Matrix a -> [Row a]
boxs = transpose
-- boxs.boxs = id

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g) 

nodups :: Eq a =>  [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs
