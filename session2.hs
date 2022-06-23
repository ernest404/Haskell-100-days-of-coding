-- A class is a collection of types that support certain overloaded operations called methods.
-- Defining Your Own Types
-- Num numeric types
-- Contains types whose values are numeric, and as such can be processed using the following six methods:  +, -, *, negate, abs, signum.

-- Integral – integral types
-- Contains numeric types whose values are integers and as such support the methods of integer division and integer remainder: div and mod

-- Fractional – fractional types
-- Contains numeric types whose values are decimal/fractions and as such support the methods of fractional division and fractional reciprocation: / and recip

-- Eq equality types
-- Contains types whose values can be compared for equality and inequality using the following two methods == (is equal) and /=(is not equal).

-- Ord ordered types
-- Contains types that are ordered, and as such can be compared using these methods: <, <=, >, and >= and the functions min, max, and compare.

-- Enum
-- Enum is used in enumerations and lets you use syntax such as [Red .. Yellow].

-- Show - showable types
-- Contains types whose values can be converted into strings of characters using the method show, show :: a -> String.

-- Read - readable types
-- Contains types whose values can be converted from strings of characters using the following method: read :: String -> a

-- Defining Your Own Types
-- 1. Type Declarations 
-- The simplest way of declaring a new type by introducing a new name for an existing type, using the type mechanism of Haskell.

-- type String = [Char]

type Pos = (Int, Int)

-- 2.Data declarations
-- A completely new type, as opposed to a synonym for an existing type, can be declared by specifying its values using the data mechanism of Haskell
data DateInfo = Date Int Int Int deriving (Show, Eq)

dateOfBirth = Date 28 7 1996

data Student = StudentInfo String DateInfo Int String deriving Show

student_1 = StudentInfo "Ernest" (Date 28 7 1996) 25 "Computer Science"

-- 3.Newtype declarations
-- If a new type has a single constructor with a single argument, then it can also be declared using the newtype mechanism

newtype StudentName = Name String

student2 = Name "Alice"

type StudentName' = String -- type declaration

data StudentName'' = Name' String 

-- Records
-- Store more info

data StudentR = Student' { firstName :: String,
                           secondName :: String,
                           dob :: DateInfo,
                           course :: String,
                           year :: Int,
                           grade :: Char
} deriving (Show, Eq)

student3  = Student' "Emily" "Jones" (Date 28 7 1998) "Medicine" 5 'A' 
student4  = Student' "Emily" "Jones" (Date 28 7 1998) "Medicine" 5 'A'


-- Functions

-- Operators == infix  2 + 2 to make operate a posfix function (+) 2 2
-- regular functions == postfix mod 2 2 to make it infix 2 `mod` 2

-- functionName a
-- functionName a b

-- Predefined function
-- These are functions found in various Haskell libraries and modules. Example the standard prelude library.

-- The main function
-- The main function serves as the starting point for program execution. It usually controls program execution by directing the calls to other functions in the program. A program usually stops executing at the end of main, although it can terminate at other points in the program for a variety of reasons.

-- User defined Functions
-- You can also define your own functions.

double :: Num a => a -> a
double x = x * 2

quadruple :: Num a => a -> a
quadruple x = double(double x)


doNothing :: Int -> Int
doNothing x = x

-- Functions with multiple arguments

add :: Num a => (a,a) -> a
add (x,y) = x + y

-- curried function to implement multiple arguments
add' :: Num a => a -> a -> a
add' x y = x + y 

-- Polymorphic functions
-- A function that can be applied to arguments of any type. This is made precise by inclusion of a type variable in type definition.

length' :: [a] -> Int
length' xs = length xs