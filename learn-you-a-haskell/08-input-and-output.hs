-- main = putStrLn "hello, world"

-- ghc --make .\08-input-and-output.hs
-- .\08-input-and-output.exe

-- :t putStrLn
-- putStrLn :: String -> IO ()

{-

 An I/O action is something that, when performed, will carry out an action
 with a side-effect (that's usually either reading from the input or printing
 stuff to the screen) and will also contain some kind of return value inside
 it. Printing a string to the terminal doesn't really have any kind of
 meaningful return value, so a dummy value of () is used.

 An I/O action will be performed when we give it a name of main and then run
 our program.


-}

-- main = do
--     putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn ("Hey " ++ name ++ ", you rock!")


-- ghci> :t getLine
-- getLine :: IO String
-- etLine is an I/O action that contains a result type of String.

{-

To run a program you can either compile it and then run the produced executable file by doing

ghc --make helloworld and then ./helloworld

or you can use the runhaskell command like so:

runhaskell helloworld.hs

-}

main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

{-

the return in Haskell is really nothing like the return in most other
languages!

In imperative languages, return usually ends the execution of a method or
subroutine and makes it report some sort of value to whoever called it.

In Haskell (in I/O actions specifically), it makes an I/O action out of a pure
value.

Using return doesn't cause the I/O do block to end in execution or anything
like that. For instance, this program will quite happily carry out all the way
to the last line:

main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line

-- We can use return in combination with <- to bind stuff to names.

main = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b

So you see, return is sort of the opposite to <-.

-}

{-
 let's take a look at some functions that are useful when dealing with I/O.
 putStr "Hey, "
 putChar 't'    -- actually defined recursively with the help of putChar.
 print [1,2,3]  -- takes a value of any type that's an instance of Show

 c <- getChar   -- an I/O action that reads a character from the input.
 c <- getChar  when (c /= ' ') $ do
 rs <- sequence [getLine, getLine, getLine]

-}

{-
ghci> mapM print [1,2,3]
1
2
3
[(),(),()]
ghci> mapM_ print [1,2,3]
1
2
3
-}

{-

forever takes an I/O action and returns an I/O action that just repeats the I/O action it
got forever.

import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

-}

{-

ForM is like mapM, only that it has its parameters switched around.

import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors

-- The (\a -> do ... ) is a function that takes a number and returns an I/O action.

$ runhaskell form_test.hs
Which color do you associate with the number 1?
white
Which color do you associate with the number 2?
blue
Which color do you associate with the number 3?
red
Which color do you associate with the number 4?
orange
The colors that you associate with 1, 2, 3 and 4 are:
white
blue
red
orange

-}
