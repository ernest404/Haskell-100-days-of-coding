{-# LANGUAGE TypeApplications #-}

module Main where

main :: IO ()
main = do
  print "Assgignmen"
  print @(Maybe Int) $ myButLast []
  print $ myButLast [1]
  print $ myButLast [1,2]
  print $ myButLast [1,2,3,4]
  print $ quickSort [1,2,1,23,3]



-- | 1. How do you write Haskell multiline comments?

{-

 This is a multiline comment

-}


-- | 2. How do we load a script on ghci ?

{-

In order to load this module to ghci do:

$ ghci
λ> :l assignment.hs
[1 of 1] Compiling Main             ( assignment.hs, interpreted )
Ok, one module loaded

-}

-- | 3. Why is lazy evaluation useful in Haskell?

{-

pros:
* performance - we evaluate only what is needed
* discovers new patterns - we can create infinite data structure for example

cons:
* hard to predict space and time complexity
* we need to be careful during parallel and concurrent programming - something which we think that is evaluated in parallel
  in fact may only be evaluated partially in so called weak head normal form

-}


-- | 4. Find the last but one element of the list [1,2,3,4]

myButLast :: [a] -> Maybe a
myButLast (a:_:[]) = Just a
myButLast (x:xs)   = myButLast xs
myButLast []       = Nothing


-- | 5. What are the types of the following values?

{-

[’a’,’b’,’c’]            :: [Char]
(’a’,’b’,’c’)            :: (Char, Char, Char)
[(False,’0’),(True,’1’)] :: [(Bool, Char)]
([False,True],[’0’,’1’]) :: ([Bool], [Char])
[tail,init,reverse]      :: [[a] -> [a]]

-}

-- | 6. Fix the syntax errors in the program below, test your solution using GHCi and paste the working code here.

n = a `div` length xs
 where
   a = 10
   xs = [1,2,3,4,5]

-- | 7. Using your new knowledge of Haskell lists explain what this code does.

f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
 where
   ys = [a | a <- xs, a <= x]
   zs = [b | b <- xs, b > x]

quickSort :: Ord a => [a] -> [a]
quickSort = f

{-

The code above is an implementation of a quick sort:

We decompose a list into first element and rest of the list.
First element [x] is taken as a pivot.
ys is a list made up of values that are less than x this is placed on the left.
zs is a list made up of values that are greater than x this is placed on the right.

This happens recursively for both ys and zs untill values are sorted then combined into one list.

-}
