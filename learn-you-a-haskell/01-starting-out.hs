{- Lists
    In Haskell, lists are a homogenous data structure.
    If you want to get an element out of a list by index, use !!. The indices start at 0.
-}
-- List Comprehensions

squaresFromOneToTen :: [Integer]
squaresFromOneToTen = [x ^ 2 | x <- [1 .. 10]]

doublesFromOneToTenGreaterThan20 :: [Integer]
doublesFromOneToTenGreaterThan20 =
  [x * 2 | x <- [1 .. 10], x * 2 > 20]

-- Because strings are lists, we can use list comprehensions to process and produce strings.
removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A' .. 'Z']]

length' xs = sum [1 | _ <- xs]

-- Nested list comprehensions are also possible if you're operating on lists that contain lists
-- Running
-- let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
-- nestedComprehension  xxs
-- will give
-- [[1,3,5,3,1,5],[1,3,5,7,9],[1,1,3,1,3,3]]
nestedComprehension :: Integral a => [[a]] -> [[a]]
nestedComprehension xxs = [[x | x <- xs, odd x] | xs <- xxs]

{- Tuples

    Tuples are used when you know exactly how many values you want to combine
    and its type depends on how many components it has and the types of the
    components.

    Tuples don't have to be homogenous

    While there are singleton lists, there's no such thing as a singleton tuple.

-}

-- fst (8,11) will give 8
-- snd (8,11) will give 11

-- zip takes two lists and then zips them together into one list by joining
-- the matching elements into pairs
-- zip [1..] ["apple", "orange", "cherry", "mango"]
-- [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]

triangles :: [(Integer, Integer, Integer)]
triangles = [ (a, b, b) | a <- [1..10], b <- [1..10], c <-[1..10] ]
rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
