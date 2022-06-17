-- my first Haskell file!

main = do
    print "Hello Ernest!"

-- To compile this Use command $ ghc hello.hs
-- If the compilation was successful, GHC will have created three files:
-- 1. hello (hello.exe on Windows)
-- 2. hello.hi
-- 3. hello.o
-- the most important file is hello, which is your binary executable. Because
-- this file is a binary executable, you can simply run the file:
-- $ ./hello
-- "Hello World!"