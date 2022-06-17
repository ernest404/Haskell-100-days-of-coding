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