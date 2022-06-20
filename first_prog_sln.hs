module Main where
import Data.Time.Format.ISO8601 (yearFormat)

main :: IO()
main = do
    print "Who is the email for?"
    recipient <- getLine

    print "What is the Title?"
    title <- getLine

    print "Who is the Author?"
    author <- getLine

   

    print(createEmail recipient title author)

toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart title = "Thanks for buying " ++ title ++ "\nthanks,\n"
fromPart author = "from "++ author ++"."

createEmail recipient title author = toPart recipient ++ bodyPart title ++ fromPart author


-- Fill in the missing part of the following where clause:
-- doublePlusTwo x = doubleX + 2
-- where doubleX = __________

doublePlusTwo x = doubleX + 2
    where doubleX = x *2

-- Write functions named inc, double, and square that increment, double, and square an argument n, respectively.

inc n = n+1

double n = n*2

square n = n * n 

-- Write a function that takes a value n. If n is even, the function returns n - 2, and if
-- the number is odd, the function returns 3 × n + 1. To check whether the number is even,
-- you can use either Haskell’s even function or mod (Haskell’s modulo function).

oddEven n = if mod n 2 == 0 then n-2 else 3 * n +1

-- Quick check 3.2 Rewrite the following function to use a lambda function in place of where:
-- doubleDouble x = dubs*2
--  where dubs = x*2

doubleDouble x = (\x -> x * 2) (x*2)

-- Function with multiple arguments
add :: (Int, Int) -> Int
add (x, y) = x+y

-- Same function implemented as as curried function
add' :: Int -> Int -> Int
add' x y = x+y

-- signum function using case
-- signum :: Int -> Int
-- signum x = case x of (x < 0) -> -1
--                      (x == 0) -> 0
--                      (x > 0) -> 1


-- not function implementation
not' :: Bool -> Bool
not' x = case x of True -> False
                   False -> True                  

-- Using library functions, define a function halve :: [a] -> ([a],[a]) that splits an evenlengthed
-- list into two halves.

halve :: [a] -> ([a], [a])
halve xs = if even (length xs) then splitAt (length xs `div` 2) xs else ([], [])

-- 2. Define a function third :: [a] -> a that returns the third element in a list that contains at least
-- this many elements using:
-- a. head and tail;
third :: [a] -> a
third xs = head
-- b. list indexing !!;
-- c. pattern matching.


-- Using recursion show that multiplication * can multiplication can be reduced to repeated addition.

(*) :: Int -> Int -> Int
x * 0 = 0
x * y = x + (x * (y-1))

-- recursion on lists
