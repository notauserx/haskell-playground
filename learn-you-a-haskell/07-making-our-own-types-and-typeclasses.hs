import qualified Data.Map as M
import System.Win32 (COORD(x), SMALL_RECT (right))
import GHC.Conc (numSparks)
import Distribution.PackageDescription (FlagAssignment)
-- ues data keyword to define a type
-- The part before the = denotes the type, which is Bool. 
-- The parts after the = are value constructors.

-- data Bool' = False | True


-- Let's work out how to define a shape type

-- A shape can be a circle or a rectangle
-- A circle value constructor has three fields
--      coordinates of the center and the radius
-- A rectangle The Rectangle value constructor has four fields
--      coordinates of the upper-left and the lower-right point

data Shape1 =
      Circle1    Float Float Float
    | Rectangle1 Float Float Float Float
        deriving (Show)

-- Now when I say fields, I actually mean parameters. Value constructors are 
-- actually functions that ultimately return a value of a data type. 

{-

ghci> :t Circle  
Circle :: Float -> Float -> Float -> Shape  
ghci> :t Rectangle  
Rectangle :: Float -> Float -> Float -> Float -> Shape  

-}

-- Let's make a function that takes shapes and returns a function

surface1 :: Shape1 -> Float
surface1 (Circle1 _ _ r) = pi * r^2
surface1 (Rectangle1 x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)

demoSurface :: IO()
demoSurface = do
        let circleSurface = surface1 $ Circle1 10 20 10
        print circleSurface
        let rectSurface = surface1 $ Rectangle1 0 0 100 100
        print rectSurface


{- 
But if we try 
ghci>Circle 10 20 5

<interactive>:5:1: error:
    * No instance for (Show Shape) arising from a use of `print'
    * In a stmt of an interactive GHCi command: print it   

we add deriving (Show) at the end of a data declaration, Haskell automagically 
makes that type part of the Show typeclass

Now we can do 

ghci> Circle 10 20 5  
Circle 10.0 20.0 5.0  
ghci> Rectangle 50 230 60 90  
Rectangle 50.0 230.0 60.0 90.0

-}


-- Value constructors are functions, so we can map them and partially apply
-- them and everything.

-- map (Circle 10 20) [4,5,6,6]  

-- Our data type is good, although it could be better.

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
    abs (x2 - x1) * abs (y2 - y1)

-- Here's a function that nudges a shape
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
    Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- nudge (Circle (Point 34 34) 10) 5 10  
-- Circle (Point 39.0 44.0) 10.0  


baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)



{-

If we wanted to export the functions and types that we defined here in a 
module,

module Shapes   
( Point(..)  
, Shape(..)  
, surface  
, nudge  
, baseCircle  
, baseRect  
) where  

By doing Shape(..), we exported all the value constructors for Shape
By doing Shape, we opt not to export any value constructors


That way, someone importing our module could only make shapes by using the 
auxilliary functions baseCircle and baseRect. Data.

Map uses that approach. You can't create a map by doing Map.Map [(1,2),(3,4)] 
because it doesn't export that value constructor. However, you can make a 
mapping by using one of the auxilliary functions like Map.fromList.

Remember, value constructors are just functions that take the fields as 
parameters and return a value of some type (like Shape)

Not exporting the value constructors of a data types makes them more abstract 
in such a way that we hide their implementation. Also, whoever uses our module 
can't pattern match against the value constructors.
-}

-- Record syntax

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     --, height :: Float  
                     --, phoneNumber :: String  
                     --, flavor :: String  
                     } deriving (Eq, Show, Read)  
                     
-- By using record syntax to create this data type, Haskell automatically made 
-- these functions: firstName, lastName, age, height, phoneNumber and flavor.

{-
ghci> :t flavor  
flavor :: Person -> String  
ghci> :t firstName  
firstName :: Person -> String  
-}

data Car = Car {company :: String, model :: String, year :: Int} 
           deriving (Show)

-- Use record syntax when a constructor has several fields and it's not 
-- obvious which field is which


{-  Type parameters
        A value constructor can take some values parameters and then produce a 
        new value. For instance, the Car constructor 

        type constructors can take types as parameters to produce new types.

        If you're familiar with templates in C++, you'll see some parallels.


        data Maybe a = Nothing | Just a

        ghci> Just "Haha"  
        Just "Haha"  
        ghci> Just 84  
        Just 84  
        ghci> :t Just "Haha"  
        Just "Haha" :: Maybe [Char]  
        ghci> :t Just 84  
        Just 84 :: (Num t) => Maybe t  
        ghci> :t Nothing  
        Nothing :: Maybe a  
        ghci> Just 10 :: Maybe Double  
        Just 10.0  

-}

-- However, it's a very strong convention in Haskell to never add typeclass 
-- constraints in data declarations.

-- Why?


{-

Once again, it's very important to distinguish between the type constructor 
and the value constructor. When declaring a data type, the part before the = 
is the type constructor and the constructors after it (possibly separated by |
's) are value constructors.

-}


{-  Derived instances
        
    A typeclass is a sort of an interface that defines some behavior.
        
    A type can be made an instance of a typeclass 
        if it supports that behavior.

    Example: 
        
        The Int type is an instance of the Eq typeclass

        Eq typeclass defines behavior for stuff that can be equated. 
        And integers can be equated.

    Typeclasses are more like interfaces. We don't make data from typeclasses.
    Instead, we first make our data type and then we think about what it can 
    act like. If it can act like something that can be equated, we make it an 
    instance of the Eq typeclass. If it can act like something that can be 
    ordered, we make it an instance of the Ord typeclass.

        
-}

{-

Of course, since Person is now in Eq we can use 

let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
mikeD == mikeD

or

ghci> let beastieBoys = [mca, adRock, mikeD]  
ghci> mikeD `elem` beastieBoys 

-}


-- We can easily use algebraic data types to make enumerations and the Enum 
--and Bounded typeclasses help us with that.

data Day = Monday 
         | Tuesday 
         | Wednesday 
         | Thursday 
         | Friday 
         | Saturday 
         | Sunday
         deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Because it's part of the Show and Read typeclasses, we can convert values 
-- of this type to and from strings.

{-

ghci> show Wednesday  
"Wednesday"  
ghci> read "Saturday" :: Day  
Saturday  

-}

-- With Eq and Ord typeclasses, we can compare or equate days

-- With Bounded we can use 
-- minBound :: Day
-- maxBound :: Day

-- It's also an instance of Enum

enumTest :: IO()
enumTest = do 
    let sr = succ Monday
    print sr

    let pr = pred Sunday
    print pr

    let workingDays = [Monday .. Friday]

    print workingDays


{-

Type synonyms

Let's make a type synonym to convey some more information in the type 
declaration.

From
type PhoneBook = [(String,String)

To



-}

type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]  
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook 

-- Type synonyms can also be parameterized. 

type AssocList k v = [(k,v)]

-- Just like we can partially apply functions to get new functions, we can 
-- partially apply type parameters and get new type constructors from them.

type IntMap = M.Map Int  

-- Another cool data type that takes two types as its parameters is the 
-- Either a b type.

{-

 However, when we're interested in how some function failed or why, we usually 
 use the result type of Either a b, where a is some sort of type that can tell 
 us something about the possible failure and b is the type of a successful 
 computation. Hence, errors use the Left value constructor while results use 
 Right.

-}

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = M.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case M.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist"
        Just(state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"


lockers :: LockerMap  
lockers = M.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  


{-

lockerLookup 101 lockers
Right "JAH3I"
lockerLookup 201 lockers
Left "Locker number 201 doesn't exist"
lockerLookup 100 lockers
Left "Locker 100 is already taken!"

-}

{-  Recursive Data Structures

    As we've seen, a constructor in an algebraic data type can have several 
    (or none at all) fields and each field must be of some concrete type. With 
    that in mind, we can make types whose constructors have fields that are of 
    the same type! Using that, we can create recursive data types, where one 
    value of some type contains values of that type, which in turn contain 
    more values of the same type and so on.

-}


infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)  

infixr 5  .++  
(.++) :: List a -> List a -> List a   
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)  

-- Notice how we pattern matched on (x :-: xs). That works because pattern 
-- matching is actually about matching constructors.

testListConcat :: IO()
testListConcat = do
    let a = 3 :-: 4 :-: 5 :-: Empty  
    let b = 6 :-: 7 :-: Empty  
    let c = a .++ b  
    
    print c

{-

Now, we're going to implement a binary search tree. 
Sets and maps from Data.Set and Data.Map are implemented using trees, only 
instead of normal binary search trees, they use balanced binary search trees, 
which are always balanced.

-}


data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
            deriving(Eq, Show, Read)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
        | x == a = Node x left right 
        | x < a  = Node a (treeInsert x left) right
        | x > a  = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
            | x == a    = True
            | x <  a    = treeElem x left
            | x >  a    = treeElem x right


testTree :: IO()
testTree = do
    let nums = [8,6,4,1,7,3,5] 
    let numsTree = foldr treeInsert EmptyTree nums  
    print numsTree

    let testEight = treeElem 8 numsTree
    print testEight
    
    let testHundred = 100 `treeElem` numsTree
    print testHundred 

    
    {-  TypeClassess 102

        A quick recap on typeclasses: typeclasses are like interfaces. A 
        typeclass defines some behavior (like comparing for equality, 
        comparing for ordering, enumeration) and then types that can behave in 
        that way are made instances of that typeclass. 

        This is how the Eq class is defined in the standard prelude:

        class Eq a where  
            (==) :: a -> a -> Bool  
            (/=) :: a -> a -> Bool  
            x == y = not (x /= y)  
            x /= y = not (x == y) 

        class Eq a where, means that we're defining a new typeclass        
        called Eq. The a is the type variable

        So once we have a class, what can we do with it? Well, not much, 
        really. But once we start making types instances of that class, we 
        start getting some nice functionality.

    -}

data TrafficLight = Red | Yellow | Green  

--we're going to write up some instances by hand, even though we could derive 
-- them for types like Eq and Show. 
-- Here's how we make it an instance of Eq.

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  

-- So class is for defining new typeclasses and 
-- instance is for making our types instances of typeclasses

-- Let's make this an instance of Show by hand, too. 

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  

-- we can't do something like
-- instance Eq Maybe where  
-- Because like we've seen, the a has to be a concrete type but Maybe isn't a 
-- concrete type. 
-- we could write it out like so:
{-
instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False  

-}

-- A yes-no typeclass

class YesNo a where 
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where  
    yesno = id  

-- id is a standard library function that takes a parameter and returns the 
-- same thing,

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True 

instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True


{-
Cool, now that we have some instances, let's go play!

ghci> yesno $ length []  
False  
ghci> yesno "haha"  
True  
ghci> yesno ""  
False  
ghci> yesno $ Just 0  
True  
ghci> yesno True  
True  
ghci> yesno EmptyTree  
False  
ghci> yesno []  
False  
ghci> yesno [0,0,0]  
True  
ghci> :t yesno  
yesno :: (YesNo a) => a -> Bool  

-}


{-  The Functor typeclass

    And now, we're going to take a look at the Functor typeclass, which is 
    basically for things that can be mapped over. You're probably thinking 
    about lists now, since mapping over lists is such a dominant idiom in 
    Haskell. And you're right, the list type is part of the Functor typeclass.

    What better way to get to know the Functor typeclass than to see how it's 
    implemented?

        class Functor f where  
            fmap :: (a -> b) -> f a -> f b  

    We see that it defines one function, fmap, and doesn't provide any default 
    implementation for it.
-}

-- In fact, map is just a fmap that works only on lists.
-- instance Functor [] where  fmap = map 
-- fmap (+2) [1..3]    returns         [3,4,5]
-- map  (+2) [1..3]     also returns    [3,4,5]

{-

Types that can act like a box can be functors. You can think of a list as a 
box that has an infinite amount of little compartments and they can all be 
empty, one can be full and the others empty or a number of them can be full. 
So, what else has the properties of being like a box? For one, the Maybe a 
type. In a way, it's like a box that can either hold nothing, in which case it 
has the value of Nothing,

-}

instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub) 

testTreeFunctor :: IO()
testTreeFunctor = do
    
    let sampleTree = foldr treeInsert EmptyTree [5,7,3,2,1,7]
    print sampleTree

    let augmentedTree = fmap (+2) sampleTree
    print augmentedTree


{-  Nice! Now how about Either a b? Can this be made a functor? 
    instance Functor (Either a) where  
        
        fmap f (Right x) = Right (f x)  
        fmap f (Left x) = Left x  

-}


-- Try figuring out how Map k is made an instance of Functor by yourself!

class Functor' f where                  
    fmap' :: (a -> b) -> f a -> f b   

instance (Ord k) => Functor' (M.Map k) where
    fmap' = M.map 


-- With the Functor typeclass, we've seen how typeclasses can represent pretty 
-- cool higher-order concepts.

{-  Kinds and some type-foo 
    
    Types are little labels that values carry so that we can reason about the 
    values. But types have their own little labels, called kinds. A kind is 
    more or less the type of a type.
-}

{- Well, let's examine the kind of a type by using the :k command

ghci> :k Int  
Int :: *  

A * means that the type is a concrete type. A concrete type is a type that 
doesn't take any type parameters and values can only have types that are 
concrete types.

ghci> :k Maybe
Maybe :: * -> *

-}