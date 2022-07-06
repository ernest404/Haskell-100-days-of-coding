-- Haskell is a pure functional language meaning that functions can't change state (of anything, like the value of a variable).
-- We don't modify data rather we construct new ones with the changes applied
-- The only thing a function can do in Haskell is give us back result based on the parameters we gave it.
-- This guarantees a function will always do what it is supposed to do
-- A function that does not change state has no side effects.

-- One problem about a program that does not change state is that it is useless, just heats up the cpu.
-- We need side effects.
-- Haskell has really clever system for dealing with functions that have side-effects 
-- by neatly separating the part of our program that is pure and the part of our 
-- program that is impure. 
-- With those two parts separated, we can still reason about our pure
-- program and take advantage of all the things that purity offers, like laziness, 
-- robustness and modularity while efficiently communicating with the outside world.

main = putStrLn "hello world"

-- TO compile this: ghc --make IO
-- Linking error when trying to compile haskell code: sudo apt install libgmp-dev
-- To run program ./IO
-- We get the string output

-- :t putStrLn = putStrLn :: String -> IO ()
-- takes a string and returns an I/O action that has a result of () unit.
-- An I/O action is something that, when performed, will carry out an action with 
-- a side-effect (that's usually either reading from the input or printing stuff to the screen)
-- Printing a string to the terminal doesn't really have any kind of meaningful return value, so a dummy value of () is used

--  An I/O action will be performed when we give it a name of main and then run our program.
-- Instead of having one I/O action which is limiting we can glue several of them using the do syntax.
main = do
    putStrLn "Hello, what is your name?"
    name <-getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")  

-- reads like imperative program: 
-- name <- getLine:  perform the I/O action getLine and then bind its result value (string) to name.

-- You can think of an I/O action as a box with little feet that will go out into the real world and do something there 
-- (like write some graffiti on a wall) and maybe bring back some data. Once it's fetched that data for you, 
-- the only way to open the box and get the data inside it is to use the <- construct. And if we're taking data 
-- out of an I/O action, we can only take it out when we're inside another I/O action.

