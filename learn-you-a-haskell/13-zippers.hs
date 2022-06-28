{-
Zippers
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree = 
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  

-- Notice that W in the tree there? Say we want to change it into a P

-- Here's a pattern match version -

changeTop1:: Tree Char -> Tree Char
changeTop1  (Node p 
                left 
                (Node l 
                    (Node _ c r)
                    right
                )
            )
            =
            
            Node p 
                left 
                (Node l 
                    (Node 'P' c r)
                    right)



-- Is there a better way of doing this? How about we make our function take a 
-- tree along with a list of directions. 

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeTop :: Directions -> Tree Char -> Tree Char
changeTop (L:ds) (Node a l r) = Node a (changeTop ds l) r
changeTop (R:ds) (Node a l r) = Node a l (changeTop ds r)
changeTop [] (Node a l r) = Node 'P' l r

-- To avoid printing out the whole tree, let's make a function that takes a 
-- list of directions and tells us what the element at the destination is:

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node a l r) = elemAt ds l
elemAt (R:ds) (Node a l r) = elemAt ds r
elemAt [] (Node a _ _) = a


-- While this technique may seem cool, it can be rather inefficient, 
-- especially if we want to repeatedly change elements.
testChageTop :: IO()
testChageTop = do
    let newTree = changeTop [R,L] freeTree
    let elem = elemAt [R,L] newTree 
    print elem -- Should print 'P'

    print newTree

{-

A Trail of breadcrumbs

As we walk the tree, we'll leave a trail of breadcrumbs along the path.
We'll use Directions to represend the breadcrumbs as well and we'll call
it breadcrumbs.

-}

type Breadcrumbs1 = [Direction]

goLeft1 :: (Tree a, Breadcrumbs1) -> (Tree a, Breadcrumbs1)
goLeft1 (Node _ l _, bs) = (l, L:bs)

goRight1 :: (Tree a, Breadcrumbs1) -> (Tree a, Breadcrumbs1)  
goRight1 (Node _ _ r, bs) = (r, R:bs) 

{- 

Let's use these functions to take our freeTree and go right and then left:

ghci> goLeft1 (goRight1 (freeTree, []))  
(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])  

-}

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x  

{-

ghci> (freeTree, []) -: goRight1 -: goLeft1  
(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])  

-}

-- What if we now want to go back up in our tree?
-- From our breadcrumbs we know that the current tree is the left sub-tree of 
-- its parent and that it is the right sub-tree of its parent, but that's it.
-- It would seem that apart from the direction that we took, a single  
-- breadcrumb should also contain all other data that we need to go back up. 

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)


-- In essence, every breadcrumb is now like a tree node with a hole in it. 
-- When we move deeper into a tree, the breadcrumb carries all the information 
-- that the node that we moved away from carried except the sub-tree that we 
-- chose to focus on. 

type Breadcrumbs a = [Crumb a]  

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)  

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (l, LeftCrumb x r:bs)  = (Node x l r, bs)
goUp (r, RightCrumb x l:bs) = (Node x l r, bs)

{- 

With a pair of Tree a andBreadcrumbs a, we have all the information to rebuild the whole tree and we also have a focus on a sub-tree. This scheme also 
enables us to easily move up, left and right. Such a pair that contains a 
focused part of a data structure and its surroundings is called a zipper, 
because moving our focus up and down the data structure resembles the 
operation of a zipper on a regular pair of pants. So it's cool to make a type 
synonym as such:

-}


type Zipper a = (Tree a, Breadcrumbs a)  

-- Manipulating trees under focus

modify :: (a -> a) -> Zipper a -> Zipper a  
modify f (Node x l r, bs) = (Node (f x) l r, bs)  
modify f (Empty, bs) = (Empty, bs)  


-- We go left, then right and then modify the root element by replacing it 
-- with a 'P'. This reads even better if we use -::

-- let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')  

-- We can then move up if we want and replace an element with a mysterious 'X':

-- let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')  

-- Moving up is easy because the breadcrumbs that we leave form the part of 
-- the data structure that we're not focusing on, but it's inverted, sort of 
-- like turning a sock inside out.

-- Each node has two sub-trees, even if those sub-trees are empty trees. 
-- we can attach a tree to a leaf node

attach :: Tree a -> Zipper a -> Zipper a  
attach t (_, bs) = (t, bs)  

-- let farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft  
-- let newFocus = farLeft -: attach (Node 'Z' Empty Empty)  

-- Making a function that walks all the way to the top of the tree,

topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z = topMost(goUp z)

{-

So now we can walk around our tree, going left and right and up, applying 
modify and attach as we go along and then when we're done with our 
modifications, we use topMost to focus on the root of our tree and see the 
changes that we've done in proper perspective.

-}
