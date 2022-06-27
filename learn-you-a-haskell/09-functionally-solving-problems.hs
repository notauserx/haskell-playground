-- Reverse Polish notation calculator
-- we write 10 - (4 + 3) * 2. +, * and - are infix operators,
-- 10 4 3 + 2 * - -- infix expression looks like in RPN:

-- it really helps to think about the type declaration of a function
-- before concerting ourselves with the implementaion details.

-- How are we going to represent the stack?
-- One thing to note about this function is that it's not really fault tolerant.

import Data.List

solveRPN :: (Num a, Read a, Floating a) => String -> a
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString:xs  
