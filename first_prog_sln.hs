module Main where

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

oddEven n = if mod 2 n then n-2 else 3 * n +1