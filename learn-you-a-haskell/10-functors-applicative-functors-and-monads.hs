{-

Functors are things that can be mapped over, like lists, Maybes, trees, and 
such. In Haskell, they're described by the typeclass Functor, which has only 
one typeclass method, namely fmap, which has a type of fmap :: (a -> b) -> f a 
-> f b

We've learned by now how a lot of types (well, type constructors really) are 
instances of Functor, like [], Maybe, Either a and a Tree type that we made on 
our own. We saw how we can map functions over them for great good. In this 
section, we'll take a look at two more instances of functor, 

namely IO and (->) r.

-}

{- Let's see how IO is an instance of Functor. 

instance Functor IO where 
    fmap f action = do
        result <- action
        return f result

The result of mapping something over an IO action will be an IO action.

We can play around with it to gain some intuition.

main = do line <- getLine   
          let line' = reverse line  
          putStrLn $ "You said " ++ line' ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"  

 Here's how to rewrite this by using fmap:

 main = do line <- fmap reverse getLine  
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"  


The I/O action fmap (++"!") getLine behaves just like getLine, only that its 
result always has "!" appended to it!

If you ever find yourself binding the result of an I/O action to a name, only 
to apply a function to that and call that something else, consider using fmap

import Data.Char  
import Data.List  
  
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line

$ runhaskell fmapping_io.hs  
hello there  
E-R-E-H-T- -O-L-L-E-H  

-}

{-

Let's think about fmap's type.

fmap :: (a -> b) -> f a -> f b.

replace all the f's with  (->) r's

fmap :: (a -> b) -> ((->) r a) -> ((->) r b)

write the (->) r a and (-> r b) types as infix r -> a and r -> b, 

fmap :: (a -> b) -> (r -> a) -> (r -> b)

Does this remind you of anything? Yes! Function composition! We pipe the 
output of r -> a into the input of a -> b to get a function r -> b, which is 
exactly what function composition is about. If you look at how the instance is 
defined above, you'll see that it's just function composition. Another way to 
write this instance would be:

instance Functor ((->) r) where  
    fmap = (.)  


-- This makes the revelation that using fmap over functions is just 
-- composition sort of obvious.


-}

{- Lifting

if we write fmap :: (a -> b) -> (f a -> f b), we can think of fmap not as a 
function that takes one function and a functor and returns a functor, but as a 
function that takes a function and returns a new function that's just like the 
old one, only it takes a functor as a parameter and returns a functor as the 
result.

This is called lifting a function.


You can think of fmap as either 
    a function that takes a function and a functor and then maps that function 
    over the functor
    
    a function that takes a function and lifts that function so that it 
    operates on functors.

-}


{- Functor laws

Next up, we're going to look at the functor laws. In order for something to be 
a functor, it should satisfy some laws.

They aren't enforced by Haskell automatically, so you have to test them out 
yourself.

-}

{-

The first functor law states that if we map the id function over a 
functor, the functor that we get back should be the same as the original 
functor. i.e: fmap id = id

    ghci> fmap id (Just 3)  
    Just 3  
    ghci> id (Just 3)  
    Just 3  
    ghci> fmap id [1..5]  
    [1,2,3,4,5]  
    ghci> id [1..5]  
    [1,2,3,4,5]  
    ghci> fmap id []  
    []  
    ghci> fmap id Nothing  
    Nothing  


If we look at the implementation of fmap for, say, Maybe, we can figure out 
why the first functor law holds.

instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing 

We imagine that id plays the role of the f parameter in the implementation.
if wee fmap id over Just x result will be Just (id x), equals Just x

So now we know that if we map id over a Maybe value with a Just value 
constructor, we get that same value back.

Seeing that mapping id over a Nothing value returns the same value is trivial. 

-}

{-

The second law says that composing two functions and then mapping the 
resulting function over a functor should be the same as first mapping one 
function over the functor and then mapping the other one. 
Formally written, that means that 

fmap (f . g) = fmap f . fmap g. 

Or to write it in another way, for any functor F, the following should hold: 

fmap (f . g) F = fmap f (fmap g F).

-}

-- Let's take a look at a pathological example of a type constructor being an 
-- instance of the Functor typeclass but not really being a functor, because 
-- it doesn't satisfy the laws. 
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data CMaybe a = CNothing | CJust Int a deriving (Show)  
 
-- The C here stands for counter.

{-
ghci> CNothing  
CNothing  
ghci> CJust 0 "haha"  
CJust 0 "haha"  
ghci> :t CNothing  
CNothing :: CMaybe a  
ghci> :t CJust 0 "haha"  
CJust 0 "haha" :: CMaybe [Char]  
ghci> CJust 100 [1,2,3]  
CJust 100 [1,2,3]  
-}

-- Let's make this an instance of Functor so that everytime we use fmap, the 
-- function gets applied to the second field, whereas the first field gets 
-- increased by 1.

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter a) = CJust (counter + 1) (f a) 


-- we can even play with this a bit:

{-

ghci> fmap (++"ha") (CJust 0 "ho")  
CJust 1 "hoha"  
ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))  
CJust 2 "hohahe"  
ghci> fmap (++"blah") CNothing  
CNothing  

-}

-- Does this obey the functor laws?

{-

ghci> fmap id (CJust 0 "haha")  
CJust 1 "haha"  
ghci> id (CJust 0 "haha")  
CJust 0 "haha"  

Ah! We know that the first functor law states that if we map id over a 
functor, it should be the same as just calling id with the same functor, but 
as we've seen from this example, this is not true for our CMaybe functor. 

All the Functor instances in the standard library obey these laws, but you can 
check for yourself if you don't believe me. And the next time you make a type 
an instance of Functor, take a minute to make sure that it obeys the functor 
laws. 

-}

{-

If you think of functors as things that output values, you can think of 
mapping over functors as attaching a transformation to the output of the 
functor that changes the value. When we do fmap (+3) [1,2,3], we attach the 
transformation (+3) to the output of [1,2,3], so whenever we look at a number 
that the list outputs, (+3) will be applied to it.

-}

{-  Applicative functors

In this section, we'll take a look at applicative functors, which are beefed 
up functors, represented in Haskell by the Applicative typeclass, found in the 
Control.Applicative module.

What happens when we map a function like *, which takes two parameters, over a 
functor?

Let's take a look at a couple of concrete examples of this. If we have Just 3 
and we do fmap (*) (Just 3), what do we get? From the instance implementation 
of Maybe for Functor, we know that if it's a Just something value, it will 
apply the function to the something inside the Just. Therefore, doing fmap (*) 
(Just 3) results in Just ((*) 3), which can also be written as Just (* 3) if 
we use sections. Interesting! We get a function wrapped in a Just!

If we map compare, which has a type of (Ord a) => a -> a -> Ordering over a 
list of characters, we get a list of functions of type Char -> Ordering, 
because the function compare gets partially applied with the characters in the 
list. It's not a list of (Ord a) => a -> Ordering function, because the first 
a that got applied was a Char and so the second a has to decide to be of type 
Char.

We see how by mapping "multi-parameter" functions over functors, we get functors that contain functions inside them.  We can map functions that take 
these functions as parameters over them, because whatever is inside a functor 
will be given to the function that we're mapping over it as a parameter.

ghci> let a = fmap (*) [1,2,3,4]  
ghci> :t a  
a :: [Integer -> Integer]  
ghci> fmap (\f -> f 9) a  
[9,18,27,36]  

But what if we have a functor value of Just (3 *) and a functor value of Just 
5 and we want to take out the function from Just (3 *) and map it over Just 5?

With normal functors, we're out of luck, because all they support is just 
mapping normal functions over existing functors. Even when we mapped \f -> f 9 
over a functor that contained functions inside it, we were just mapping a 
normal function over it. But we can't map a function that's inside a functor 
over another functor with what fmap offers us. We could pattern-match against 
the Just constructor to get the function out of it and then map it over Just 
5, but we're looking for a more general and abstract way of doing that, which 
works across functors.

Meet the Applicative typeclass. It lies in the Control.Applicative module and it defines two methods, pure and <*>. 

class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b 


A better way of thinking about pure would be to say that it takes a value and 
puts it in some sort of default (or pure) context—a minimal context that still 
yields that value.

The <*> function is really interesting. 
It has a type declaration of f (a -> b) -> f a -> f b. Does this remind you of 
anything? Of course,   fmap :: (a -> b) -> f a -> f b. It's a sort of a beefed 
up fmap. Whereas fmap takes a function and a functor and applies the function 
inside the functor, <*> takes a functor that has a function in it and another 
functor and sort of extracts that function from the first functor and then 
maps it over the second one. 

When I say extract, I actually sort of mean run and then extract, maybe even 
sequence. We'll see why soon.

Let's take a look at the Applicative instance implementation for Maybe.

instance Applicative Maybe where
    pure = Just 
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something


So for Maybe, <*> extracts the function from the left value if it's a Just and 
maps it over the right value. If any of the parameters is Nothing, Nothing is 
the result.

ghci> Just (+3) <*> Just 9  
Just 12  
ghci> pure (+3) <*> Just 10  
Just 13  
ghci> pure (+3) <*> Just 9  
Just 12  
ghci> Just (++"hahah") <*> Nothing  
Nothing  
ghci> Nothing <*> Just "woot"  
Nothing  

-}
createPhoneNumber :: [Int] -> String
createPhoneNumber (a:b:c:d:e:f:g:h:i:j:k)
    = "("++ show a ++ show b ++ show c ++ ")" ++ " "
      ++ show d ++ show e ++ show f ++ "-"
      ++ show g ++ show h ++ show i ++ show j
