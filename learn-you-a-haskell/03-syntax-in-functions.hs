{- Pattern Matching

    Pattern matching consists of specifying patterns to which some data
    should conform and then checking to see if it does and deconstructing
    the data according to those patterns.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> [Char]
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- Pattern matching can also fail. If we define a function like this:

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
-- When making patterns, we should always include a catch-all
-- pattern so that our program doesn't crash if we get some unexpected input.
charName _   = "Unknown"

-- Pattern matching can be used on Tuples
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- We can use pattern matching on list comprehensions
-- let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
patMatOnListComp :: Num a => [(a, a)] -> [a]
patMatOnListComp xs = [a + b | (a, b) <- xs]

-- Lists themselves can also be used in pattern matching.
customHead :: [a] -> a
customHead []       = error "Can't call head on an empty list"
customHead (x : xs) = x

-- Let's make a customLength with pattern matching and a little recursion
customLength :: Num b => [a] -> b
customLength []       = 0
customLength (_ : xs) = 1 + customLength xs

-- call customLength "Ham"

-- Let's write a customSum
customSum :: (Num a) => [a] -> a
customSum []       = 0
customSum (x : xs) = x + customSum xs

-- There's also a thing called as patterns.
-- For instance, the pattern xs@(x:y:ys)
capital :: String -> String
capital ""           = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

{- Guards, guards!

    Whereas patterns are a way of making sure a value conforms to some form and
    deconstructing it,  guards are a way of testing whether some property of a
    value (or several of them) are true or false

-}

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

-- Note that there's no = right after the function name and its parameters,
-- before the first guard.

bmiTellWithWhere :: (RealFloat a) => a -> a -> String
bmiTellWithWhere weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

{- Let it be

    Very similar to where bindings are let bindings. Where bindings are a
    syntactic construct that let you bind to variables at the end of a
    function and the whole function can see them, including all the guards.
    Let bindings let you bind to variables anywhere and are expressions
    themselves, but are very local, so they don't span across guards.
-}

-- Let's define a function that gives us a cylinder's surface area based on
-- its height and radius:

getSurfaceAreaOfCylinder :: RealFloat a => a -> a -> a
getSurfaceAreaOfCylinder r h =
  let sideArea = 2 * pi * r * h
      toparea = pi * r ^ 2
   in sideArea + 2 * toparea

-- The form is let <bindings> in <expression>.
-- What's the difference between the where and the let bindings?
--  let bindings are expressions themselves.
--  where bindings are just syntactic constructs
--  if else statement is an expression and you can cram it in almost anywhere
--  You can also do that with let bindings.

sampleLetBinding :: Int
sampleLetBinding = 4 * (let a = 9 in a + 1) + 2 -- 42

--  You can use let to introduce functions in local scoppe

squaresOfTwoThreeAndFive =
  [let square x = x * x in (square 2, square 3, square 5)]

-- If we want to bind to several variables inline, we obviously can't align
-- them at columns. That's why we can separate them with semicolons.
bindSeveralVariablesInline :: (Integer, [Char])
bindSeveralVariablesInline =
  (let a = 1; b = 2; c = 3 in a * b * c, let foo = "Hey"; bar = "there" in foo ++ " " ++ bar)

-- You can pattern match with let bindings
patternMatchWithLetBindings =
  (let (a, b, c) = (1, 2, 3) in a + b + c) * 100

-- You can also put let bindings inside list comprehensions

calcBmis :: RealFloat a => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


-- The names defined in a let inside a list comprehension are visible to the
-- output function (the part before the |) and all predicates and sections
-- that come after of the binding

calcBmisForFatPeople :: (RealFloat a) => [(a, a)] -> [a]
calcBmisForFatPeople xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- We can't use the bmi name in the (w, h) <- xs part because it's defined
-- prior to the let binding.


{- Case expressions

    Like the name implies, case expressions are, well, expressions, much like
    if else expressions and let bindings. Not only can we evaluate expressions
    based on the possible cases of the value of a variable, we can also do
    pattern matching.

    Oh yeah, pattern matching on parameters in function definitions!
    Well, that's actually just syntactic sugar for case expressions.

    case expression of pattern -> result
                   pattern -> result
                   pattern -> result
                   ...



-}

-- Whereas pattern matching on function parameters can only be done when defining
-- functions, case expressions can be used pretty much anywhere.

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty."
                                               [x] -> "a singleton list."
                                               xs  -> "a longer list."

{-

  we could have also defined this like so

  describeList :: [a] -> String
  describeList xs = "The list is " ++ what xs
      where what [] = "empty."
            what [x] = "a singleton list."
            what xs = "a longer list."


-}
