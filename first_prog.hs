-- The key issue is that this code is in one big monolithic function named messyMain. The
-- advice that it’s good practice to write modular code is fairly universal in software, but in
-- Haskell it’s essential for writing code that you can understand and troubleshoot.
messyMain :: IO()
messyMain = do
    print "Who is the email for?"
    recipient <- getLine
    print "What is the Title?"
    title <- getLine
    print "Who is the Author?"
    author <- getLine
    print ("Dear " ++ recipient ++ ",\n" ++
        "Thanks for buying " ++ title ++ "\nthanks,\n" ++
        author )
-- Everything works fine, but it’d be much easier to work with if this code was broken up a bit
   