{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import           System.Win32 (COORD (x))
{- Recursion

    Recursion is important to Haskell because unlike imperative languages,
    you do computations in Haskell by declaring what something is instead
    of declaring how you get it.

    That's why there are no while loops or for loops in Haskell and instead
    we many times have to use recursion to declare what something is.
-}

customMaximum :: (Ord a) => [a] -> a
customMaximum []       = error "maximum of a empty list"
customMaximum [x     ] = x
customMaximum (x : xs) = max x (customMaximum xs)

replicate' :: (Ord t, Num t) => t -> a -> [a]
replicate' n x | n <= 0    = []
               | otherwise = x : replicate' (n - 1) x

-- We used guards here instead of patterns because we're testing for a
-- boolean condition.

takeN :: (Num t, Ord t) => t -> [a] -> [a]
takeN n _ | n <= 0 = []
takeN _ []         = []
takeN n (x : xs)   = x : takeN (n - 1) xs

-- takeN 3 [1,1,1,2,2,2] should output [1,1,1]


-- Here is a reverse implementation

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]


-- Here is a zip implementation
-- zip [1,2,3] [2,3] returns [(1,2),(2,3)]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


-- Here is a quick sort implementation

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    let leftPart = [a | a <- xs, a <=x]
        rightPart  = [a | a <- xs, a > x]
    in qsort leftPart ++ [x] ++ qsort rightPart

-- qsort [10,2,5,3,1,6,7,4,2,3,4,8,9]






