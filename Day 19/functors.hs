import Data.Functor
-- Pure = programs are functions in a mathematical sense, ie a map of input to output with no effects on the outside world.
-- Impure = programs can have effect on outside world.
-- We do need effects (interaction with outside world) for our programs to be useful.
-- To combine the two approaches we use monads, to allow programs to interact with outside world but still program with no side effects.

-- Abstracting programming patterns.

inc :: [Int] -> [Int]
inc [] = []
inc (x:xs) = x+1 : inc xs -- we increment the first element of the list and recursively deal with the rest of the elements in the same way.


sqr :: [Int] -> [Int]
sqr [] = []
sqr (x:xs) = x^2 : sqr xs -- we square the first element of the list and recursively deal with the rest of the elements in the same way.

-- These two functions have similar patterns and can be abstracted by the map function which applies a function to each element of a list in turns.
map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- We can define the inc and sqr function in terms of map as: 

inc' = map (+1)
sqr' = map (n^2)

-- As map, monad is a way of abstracting common patterns.

-- Generalizing further
-- The technique of mapping functions is not limited to list you can map to other data structures like trees, Maybe types
-- This is the purpose of functors.

-- Functor class definition:

class Functor f where -- For a type f to be an instance of the class Functor it must support the fmap method.
    fmap :: (a -> b) -> fa -> fb
-- f represents a parameterized type([a]) like list and trees that support the fmap method.
-- Functor generalizes mapping for any parameterized type f

-- List Functor
instance Functor [] where --[] represents a parameterised type of list 
    -- fmap :: (a->b) -> [a] -> [b]: commented out because haskell doesn't allow type declarations in instance definitions.
    -- this means because this is exactly what map does.
    fmap = map


-- Maybe Functor
data Maybe a = Nothing | Just a -- returns a nothing when there is an error or no value and just the value when there is a value.

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap g Nothing = Nothing
    fmap g (Just x) = Just (g x)


-- Tree Functor 
data Tree a = Leaf a | Node (Tree a) (Tree a)

merkle :: Tree Int
merkle = Node (Leaf 1) (Leaf 2)

instance Functor Tree where
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- IO Functor
instance Functor IO where
    -- fmap :: (a->b) -> IO a -> IO b
    fmap g io_action = do
        result <- io_action -- gives us the value from an IO action
        return (g result) -- return converts result of type a into a an IO action.

-- Why use Functors
-- We can use same name, fmap for functions that are essentially the same.
-- We can define a generic function that can be applied to many types that implement the functor class.
inc = fmap (+1)

-- Applicative Functor

-- Generalising fmap to apply functions to number different parameters.

{-
fmap0 :: a -> fa : fmap on a function with no arguments(constant) returns same value as of type class functor.
fmap1 :: (a ->b) -> fa -> fb
fmap2 :: (a -> b -> c) -> fa -> fb -> fc
fmap3 :: (a -> b -> c -> d) -> fa -> fb -> fc -> fd
-}

-- We can generalize these into two methods under applicative type class.

pure :: a -> f a -- pure converts a value of type a into a data structure that implements the functor class like list, maybe and Tree
(<*>) :: f(a -> b) -> f a -> f b --infix operator.-- generalized form of function application, by taking a data structure f(a->b) and another datastructure fa to produce a data  structure fb.

-- We use the two functions apply a function of any kind to a Functor data structure
pure g <*> x <*> y <*> z --This is applicative style: we are applying function g to x then to y then z

-- same as:

g x y z = (((g x) y) z)

-- Defining fmap0 by generalizing
fmap0 :: a -> fa
fmap0 = pure

-- Defining fmap1
fmap1 :: (a -> b) -> fa -> fb
fmap1  g x = pure g <*> x

{-
g = a->b
x = fa

pure g = f(a->b)
x = fa

<*> = f(a->b) -> fa -> fb takes f(a->b) and fa to give us fb
-}
-- Defining fmap2
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y

{-
g = (a -> b -> c)
x = fa
y = fb

pure g = f(a -> b -> c)
x = fa
y = fb
<*> = f(a->b) -> fa -> fb takes f(a->b->c) fa, fb to give us fc

-}

-- Applicative Functors definition- generalized functors to apply functions with more with more than one arguements.
class Functor f => Applicative f where --In order to be part of applicative type class we have to be an instance of the Functor class and implement the two functions below.
    pure :: a -> f a
    (<*>) :: f (a->b) -> f a -> f b

-- Example: Maybe applicative: 
instance Applicative Maybe where
    -- pure :: a -> Maybe a : Maybe is already an instance of f functor(pure :: a -> f a)
    pure x = Just x
    -- (<*>) :: Maybe (a->b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    (Just g) <*> mx = fmap g mx

-- Gives us ability to apply pure functions to arguments that have ability to fail. Exception in programming.
just_3 = pure (+) <*> Just 1 <*> Just 2

nothing = pure (+) <*> Nothing <*> Just 2


-- Example: list applicative: 
instance Applicative [] where
    -- pure :: a -> [a]
    pure x = [x]
    --  <*> :: [a-b] -> [a] -> [b] ......is same as saying  <*> :: [] (a-b) -> [] a -> [] b 
    gs <*> xs = [g x | g <- gs, x <- xs]

pure (*) <*> [1,2] <*> [3,4] -- give us all possible values of the function apply

-- Give ability to non-deterministically apply pure functions to multivalued arguements

-- In general applicative functors apply functions to effectfull arguments.