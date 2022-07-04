import Data.List (transpose)
import Distribution.Simple.Command (OptDescr(BoolOpt))

-- Basic Declarations

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char
-- Grid = [[Char]]

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

-- boxes helper function: extracts boxs as list of rows. A box is store as a row.
boxes :: Matrix a -> [Row a]
boxes = transpose
-- boxes.boxes = id

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxes g) 

nodups :: Eq a =>  [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

solve :: Grid ->[Grid] --takes a sodoku puzzle grid and returns a list of all possible solution grids.
solve = filter valid.collapse.choices --this is function composition it is same as saying solve g = filter valid (collapse(choices g))

type Choices = [Value] -- Choices a list of values 1 to 9

choices :: Grid -> Matrix Choices --choices function takes a puzzle grid and returns a matrix of possible choices for all cells
choices g = map (map choice) g where --double map is applying the mini function choice to every cell of the puzzle grid(a lisi of a list). map.map :: (a -> b) -> [[a]] -> [[b]], [[a]] represents the input grid and [[b]] the output grid.
        choice v = if v=="." then ['1'..'9'] else [v] --choice is a small function that replaces empty cells (representated by .) with a list of all possible values. Else if the cell has a value it returns the existing value as a list because we are returning a Matrix of choices.

collapse :: Matrix [a] -> [Matrix a] -- Gives a list all possible combination of choices for all the cells.
collapse m = cp (map cp m) --collapse each row then collapse each column


cp :: [[a]] -> [[a]]
-- returns all possible combination of values from lists.
cp [] = [[]]--base case
cp (xs:xss) = [y:ys| y <- xs, ys <- cp xss] 
-- Decompose a list of lists into the first list and other lists in the list of lists
-- draw all possible values from the first list
-- Draw all possible values from the rest of lists. Recursively draw the first list ys from the rest of the lists in the list and draw all possible values from it.

--  in principle this sudoku solver works but has so many computation that will never give as results in our life time.
-- The solution is to reduce these choices by pruning out choice that don't meet the rule.

prune :: Marix Choices -> Matrix Choices
                 