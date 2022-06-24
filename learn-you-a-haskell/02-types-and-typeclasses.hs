{-
Haskell has a static type system.
Haskell has type inference.
Use the :t command followed by an expression to get its type

:t (==)
(==) :: (Eq a) => a -> a -> Bool
Notice the => symbol.
Everything before the => symbol is called a class constraint.
-}
{-
Type variables
:t head
head :: [a] -> a
Hmmm! What is this a? Is it a type?
Remember that we previously stated that types are written in capital case,
so it can't exactly be a type.
Because it's not in capital case it's actually a type variable.
That means that a can be of any type.
This is much like generics in other languages, only in Haskell it's much
more powerful because it allows us to easily write very general functions
if they don't use any specific behavior of the types in them. Functions
that have type variables are called polymorphic functions.
-}
{-
Typeclasses 101

A typeclass is a sort of interface that defines some behavior.
If a type is a part of a typeclass, that means that it supports and
implements the behavior the typeclass describes.

Eq, Ord, Read, Show
-}
