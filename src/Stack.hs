module Stack(Stack,empty, isEmpty, pop, push) where
--------------------------------------------------------------------------------- 
-- Tue Nov 20 16:50:42 2018 
-- stack in haskell
-- 7 line of code, you can have a stack
-------------------------------------------------------------------------------- 
newtype Stack a = Stk a deriving (Show)
empty            = Stk []
push x (Stk xs)  = Stk (x:xs)
pop (Stk x:xs)   = Stk xs
top (Stk x:xs)   = x
isEmpty (Stk xs) = null xs 
get (Stk xs)     = xs

