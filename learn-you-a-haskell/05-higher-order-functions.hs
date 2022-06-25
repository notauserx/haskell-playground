{- Higher Order Functions

    Haskell functions can take functions as parameters and return functions
    as return values.

    A function that does either of those is called a higher order function.

-}

{- Curried functions

    Every function in Haskell officially only takes one parameter.
    All the functions that accepted several parameters so far have been
    curried functions.

    Doing max 4 5 first creates a function that takes a parameter and returns
    either 4 or that parameter, depending on which is bigger. Then, 5 is
    applied to that function and that function produces our desired result.

    Putting a space between two things is simply function application.

    The space is sort of like an operator and it has the highest precedence.

    if we call a function with too few parameters,
        we get back a partially applied function,

-}

-- Let's look at this simple example

multThree :: (Num a) => a-> a-> a-> a
multThree x y z = x * y * z

{- What really happens when we do multThree 3 5 9 or ((multThree 3) 5) 9?

    First, 3 is applied to multThree, because they're separated by a space.
    That creates a function that takes one parameter and returns a function.

    Then 5 is applied to that, which creates a function that will take a
    parameter and multiply it by 15.

    9 is applied to that function and the result is 135 or somethin
-}


compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- Infix functions can also be partially applied by using sections. To section
-- an infix function, simply surround it with parentheses and only supply a
-- parameter on one side.

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)





{- Some higher-orderism is in order

    Functions can take functions as parameters and also return functions.
-}


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


-- First of all, notice the parentheses in the type declaration.
-- They indicate that the first parameter is a function that takes something
-- and returns that same thing.

-- Lets implement a really useful function that's in the standard library.
-- It's called zipWith. It takes a function and two lists as parameters and
-- then joins the two lists by applying the function between corresponding
-- elements.

zipWith' :: (a-> b-> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- zipWith' (+) [4,2,5,6] [2,6,2,3]
-- [6,8,7,9]
-- zipWith' max [6,3,2,1] [7,3,1,5]
-- [7,3,2,5]


-- Here is am implementation of flip
-- Try flip' zip [1,2,3,4,5] "hello"

flip' :: (a -> b -> c) -> b -> a-> c
flip' f x y = f y x


{-  Maps and Filters
        Map takes a function and a list and applies that function to every
            element in the list, producing a new list.

        Filter is a function that takes a predicate and a list and then
            returns the list of elements that satisfy the predicate.

        A predicate is a function that tells whether something is true or not
-}


map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

-- map (+3) [1,5,3,1,6]  returns [4,8,6,4,9]

-- Map ould be achieved with a list comprehension.
-- map (+3) [1,5,3,1,6] is the same as writing [x+3 | x <- [1,5,3,1,6]]
-- However using map is much more readable where you are dealing with maps
-- of maps

-- Here is am implementation of filter

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x:filter' f xs
    | otherwise = filter' f xs


-- filter' (>3) [1,5,3,2,1,6,4,3,2,1] returns [5,6,4]

runFilterExamples :: IO()
runFilterExamples = do
    putStrLn "Running: filter' odd [1..10]"
    let a = filter' odd [1..10]
    print a -- this is equivalent to putStrLn $ show a


-- quickSort with Filter

qSort' :: (Ord a) => [a] -> [a]
qSort' [] = []
qSort' (x:xs) =
    let smaller = filter (<x) xs
        notSmaller = filter (>=x) xs

    in qSort' smaller ++ [x] ++ qSort' notSmaller



-- Again, Haskell's property of laziness is what makes this possible.
-- We can map over and filter an infinite list, because it won't actually
-- map and filter it right away.
sumOfOddSquaresSmallerThanTenThousand :: Integer
sumOfOddSquaresSmallerThanTenThousand =
    sum (takeWhile (<10000) (filter odd (map(^2)[1..])))



collatzSequence :: Integral a => a -> [a]
collatzSequence 1 = [1]
collatzSequence n
    | even n = n:collatzSequence(div n 2)
    | odd n  = n:collatzSequence(n*3 + 1)


numLongChains :: Int
numLongChains = length (filter isLong (map collatzSequence [1..100]))
    where isLong xs = length xs > 15


{- Lambdas
    Lambdas are basically anonymous functions that are used because we need
    some functions only once.

    To make a lambda,
      We write a \
        because it kind of looks like the greek letter lambda
      Then we write the parameters, separated by spaces.
      After that comes a -> and then the function body.

-}

numLongChainsWithLambda :: Int
numLongChainsWithLambda = length (filter (\xs -> length xs > 15)
                                    (map collatzSequence [1..100]))


-- Like normal functions, lambdas can take any number of parameters
lambpdaWithTwoParameters :: [Double]
lambpdaWithTwoParameters = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

-- You can pattern match in Lambdas
-- The only difference is that you can't define several patterns for one
-- parameter, like making a [] and a (x:xs) pattern for the same parameter

-- If a pattern matching fails in a lambda, a runtime error occurs

lambdaWithPatternMatching :: [Integer]
lambdaWithPatternMatching = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]


{- Only folds and horses

    These functions are called folds. They're sort of like the map function,
    only they reduce the list to some single value.

    A fold takes
        a binary function,
        a starting value (I like to call it the accumulator) and
        a list to fold up.

    foldl function, also called the left fold,
        folds the list up from the left side



-}

-- Let's implement sum using fold instead of recursion

sumWithFoldLeft :: (Num a) => [a] -> a
-- sumWithFoldLeft = foldl(\ acc x -> acc + x) 0
-- more succintly
sumWithFoldLeft = foldl (+) 0


elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl (
    \ acc x -> (x == y) || acc)
    False
-- elem' y ys = foldl (\ acc x -> if x == y then True else acc) False ys


-- The right fold, foldr works in a similar way to the left fold, only the
-- accumulator eats up the values from the right.
-- the right fold's binary function has the current value as the first
-- parameter and the accumulator as the second one (so \x acc -> ...).

countUpper' :: String -> Int
countUpper' [] = 0
countUpper' (x:xs) = foldr(
    \ x acc -> if x `elem` ['A'..'Z']
                    then acc + 1
                    else acc)
                0 xs

-- Here is an example of fold returning a list

reverseWithFold :: String -> String
reverseWithFold = foldl(\ acc x -> x : acc) []


-- filterWithFold (<4) [1,2,3,4,5,6,7,8] returns [1,2,3]
filterWithFold :: (a -> Bool) -> [a] -> [a]
filterWithFold p = foldr(\x acc -> if p x then x : acc else acc) []


-- The foldl1 and foldr1 functions work much like foldl and foldr, only you
-- don't need to provide them with an explicit starting value. They assume
-- the first (or last) element of the list to be the starting value and then
-- start the fold with the element next to it.

-- scanl and scanr are like foldl and foldr, only they report all the
-- intermediate accumulator states in the form of a list.
--      scanl (+) 0 [3,5,2,1]
--      returns[0,3,8,10,11]

-- Scans are used to monitor the progression of a function that can be
-- implemented as a fold.
-- Let's answer us this question: How many elements does it take for the sum
-- of the roots of all natural numbers to exceed 1000?

sqrtSums :: (Ord a, Floating a, Enum a) => a -> Int
sqrtSums n = length (takeWhile (<n) (scanl1  (+) (map sqrt [1..]))) + 1



{- Function application with $
    Alright, next up, we'll take a look at the $ function,
        also called function application.

        ($) :: (a -> b) -> a -> b
        f $ x = f x

    Whereas normal function application (putting a space between two
    things) has a really high precedence,
    the $ function has the lowest precedence

    Function application with a space is left-associative.
        f a b c is the same as ((f a) b) c))

    Function application with $ is right-associative.
-}
-- we can rewrite sum (map sqrt [1..130]) as sum $ map sqrt[1..130]

-- $ means that function application can be treated just like another
-- function. That way, we can, for instance, map function application
-- over a list of functions.
-- map ($ 3) [(4+), (10*), (^2), sqrt]

{- Function composition
    In mathematics, function composition is an operation ∘ that takes two
    functions f and g, and produces a function

        h = g ∘ f such that h(x) = g(f(x)).

    In Haskell, function composition is pretty much the same thing.
    We do function composition with the . function,

        (.) :: (b -> c) -> (a -> b) -> a -> c
        f . g = \x -> f (g x)
-}

turnIntoNegetiveNumbers :: [Integer] -> [Integer]
turnIntoNegetiveNumbers = map (negate . abs) --[5,-3,-6,7,-3,2,-19,24]

-- Function composition is right-associative,
-- so we can compose many functions at a time.
-- The expression f (g (z x)) is equivalent to (f . g . z) x.

-- map (negate . sum . tail) [[1..5],[3..6],[1..7]]
-- returns [-14,-15,-27]

-- But what about functions that take several parameters?
-- We usually have to partially apply them just so much that each function
-- takes just one parameter.

--      sum (replicate 5 (max 6.7 8.9))     -- can be rewritten as
--      (sum . replicate 5 . max 6.7) 8.9   -- or as
--      sum . replicate 5 . max 6.7 $ 8.9.

-- Another common use of function composition is defining functions in the
-- so-called point free style

-- Take for example this function that we wrote earlier:

--  sum' :: (Num a) => [a] -> a
--  sum' xs = foldl (+) 0 xs

-- Writing the function as sum' = foldl (+) 0 is called writing it in point
-- free style.

fn :: Double -> Integer
fn = ceiling . negate . tan . cos . max 50

-- However, many times, writing a function in point free style can be less
-- readable if a function is too complex. That's why making long chains of
-- function composition is discouraged.

-- he prefered style is to use let bindings to give labels to intermediary
-- results or split the problem into sub-problems and then put it together.

{-
oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit
-}
