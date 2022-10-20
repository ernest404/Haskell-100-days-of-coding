-- Monads 
-- Just like functors involve observing a common pattern and abstracting it out.

-- Example: a simple evaluator

data Expr = Val Int | Div Expr Expr deriving Show

eval' :: Expr -> Int
eval' (Val n) = n
eval' (Div x y) = eval x `div` eval y

-- problem is div does not deal with division by 0, which will make the program fail in such a scenario.

-- First define a safe version of div, handles division by 0
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

-- a new eval function (complex)

eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) = case eval x of
    Nothing -> Nothing
    Just n -> case eval y of
        Nothing -> Nothing
        Just m -> safediv n m

-- a new simpler eval function (which doesn't work)

eval'' :: Expr -> Maybe Int
eval (Val n) = pure n
eval (Div x y) = pure safediv <*> eval x <*> eval y                                                                                                                                                                                             

{-
but eval function does not fit the style of applicative.
because for pure safediv we have Int -> Int -> Maybe Int but what is required is Int -> Int -> Int

-}

-- Note there is a common pattern in the eval function     case mx of 
--                                                             Nothing -> Nothing
--                                                             Just x -> f x
-- mx is a some maybe value while f is some function 

-- We can abstract this pattern using the >>= bind operator
{-mx >>= f = case m x of 
    Nothing -> Nothing
    Just x -> f x

declaration of >>=:

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

but general declaration is:
(>>=) :: Monad m => m a -> (a -> m b) -> m b
-}
-- Can be used with maybe types as it is an instance of type class Monad 

eval''' :: Expr -> Maybe Int
eval''' (Val n) = Just n
eval''' (Div x y) = eval x >>= (\n -> eval y >>= (\m -> safediv n m)) -- if you want to evaluate division, first eval x if it is sucessful then feed result to the lambda function with argument n which calls for eval of y if successful

-- The Pattern is:

{-
m1 >>= \x1 ->
    m2 >>= \x2 ->
        ...
        ...
        mn >>= \xn ->
            f x1 x2 ... xn

It only suceeds if all its components suceeds, while error is propagated to the final result.
-}

-- This pattern is so common that Haskell has a simple syntax for it : do notation

{-do
    x1 <- m1
    x2 <- m2
    ....
    ...
    f x1 x2 ... xn

syntatic sugar of >>= bind
-}

-- simple eval using do notation
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do n <- eval x --evaluate x get Maybe value n 
                    m <- eval y --evaluate y get Maybe value m
                    safediv n m -- if both are sucessful we apply the function to n m (Monadic type class)

-- It is a pure program that is very proceedural.

-- Monads: Class of applicative functors that support the bind operator and return

class Applicative m => Monad m where 
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a -- provides a way to get into the monad
    return = pure -- return is define as pure. because pure has the same definition and also for a value to be a monad it has to be an applicative

-- Example: Maybe Monad 
instance Monad Maybe where -- this instance is what makes the do notation work with any type.
    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= f = Nothing
    Just x >>= f = f x

-- Example: List Monad
instance Monad [] where
    -- (>>=) :: [a] -> (a -> [b]) -> [b]

    xs >>= f = concat (map f xs) --takes a list of lists and compresses into a list
    -- xs >>= f  = [y|x<-xs, y <- f x] 


pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)

-- same as pairs xs ys = [(x,y) | x <- xs, y <- ys]
-- do is recommended because it can be generalized to work with other types other than lists.


-- State Monad.

-- example

type State = ...

type ST a = State -> (a,State) -- state transformer type a takes current version of state and gives out a result value of type a and modified state

-- To generalize this we use currying

-- A function that takes Char and returns a State Transformer and can be expanded as follows

-- :: Char -> ST Int but  type ST a = State -> (a,State)
-- func :: Char -> State -> (Int, State)

-- We want to make type ST into an instance of Monad class
-- However, in order to make things into instance classes they to be data declaration.
-- In data declaration there has to be a data constructor, here we will use S, this constructor has no use it is just there to make the type checker happy
-- data ST a = S (State -> (a, State))

--  to make sure this dummy constructor doesn't have runtime overhead, we use newtype instead since it is only used with one data constructs. So it constrains it during runtime.
data ST a = S (State -> (a, State))

-- to get rid of the dummy constructor S
app :: ST a -> state -> (a, State)
app (S st) s = st s --app takes S - state constructor, st -state transformer and current state as input and give out the state transformer st and modified state as output, getting rid of state constructor S

instance Monad ST where
    -- return :: a -> ST a
    return x = S (\s -> (x, s))

    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S(\s->
        let (x,s') = app st s --gives us a value x and a new state
        in app(f x) s')


{-
x = a
s = state
S (\s -> (x, s)) = ST a
return passes things unchanged.
-}

{-
st = ST a
f = (a -> ST b)
S(\s->(b, State))
-}


