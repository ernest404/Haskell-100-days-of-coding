-- 1. Type declaration 
-- Introducing a new name for existing types.
-- Importance: It makes type declarations easier to read.

type String' = [Char] -- instead of list of character we call this a String

hello :: String'
hello = "Hello World"

type Coordinates = (Float, Float)

lamu :: Coordinates
lamu = (2.2696, 40.9006)

-- Type declarations can also be parameterized.
type Pair a = (a, a)

lamu2 :: Pair Float
lamu2 = (2.2696, 40.9006)

-- Type declarations can be nested:
type Pos = (Int, Int)
type Trans = Pos -> Pos

-- 2.Data declarations
-- Declares a completely new type.
data Bool' = Uongo | Ukweli deriving

