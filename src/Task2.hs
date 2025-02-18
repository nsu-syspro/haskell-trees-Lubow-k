{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..), torder, Order (InOrder))

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show, Eq)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
--
compare :: Ord a => Cmp a
compare x y
  | x == y = EQ
  | x < y  = LT
  | otherwise = GT

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
--
listToBST :: Cmp a -> [a] -> Tree a
listToBST cmp = foldr (tinsert cmp) Leaf 

-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
--
bstToList :: Tree a -> [a]
bstToList = torder InOrder Nothing

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
--
isBST :: Cmp a -> Tree a -> Bool
isBST cmp tree = isSortedAscList cmp (bstToList tree) 

isSortedAscList :: Cmp a -> [a] -> Bool 
isSortedAscList cmp list = foldr (checkSorted cmp) True (zip list (tail list))

checkSorted :: Cmp a -> (a, a) -> Bool -> Bool
checkSorted cmp (current, next) acc = cmp current next == LT && acc


-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
--
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup _ _ Leaf   = Nothing
tlookup cmp v (Branch x left right) 
  | cmp v x == EQ  = Just x
  | cmp v x == GT  = tlookup cmp v right
  | otherwise      = tlookup cmp v left

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
--
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert _ v Leaf = Branch v Leaf Leaf
tinsert cmp v (Branch x left right) 
  | cmp v x == LT   = Branch x (tinsert cmp v left) right
  | cmp v x == GT   = Branch x left (tinsert cmp v right)
  | otherwise       = Branch v left right 


-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
--
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete _ _ Leaf = Leaf
tdelete cmp v (Branch x left right) 
  | cmp v x == LT   = Branch x (tdelete cmp v left) right
  | cmp v x == GT   = Branch x left (tdelete cmp v right)
  | otherwise       = ndelete cmp (Branch v left right)


ndelete :: Cmp a -> Tree a -> Tree a
ndelete _ Leaf                 =  error "deleting non-existent element"
ndelete _ (Branch _ Leaf Leaf) =  Leaf         -- no children 
ndelete _ (Branch _ Leaf rBranch) = rBranch    -- only right child
ndelete _ (Branch _ lBranch Leaf) = lBranch    -- only left child
ndelete cmp (Branch _ lBranch rBranch) = Branch minVal lBranch (tdelete cmp minVal rBranch)  -- two children (find min on the right)
  where minVal = findMin rBranch


findMin :: Tree a -> a
findMin Leaf = error "deleting leaf"
findMin (Branch _ (Branch x left right) _) = findMin (Branch x left right)
findMin (Branch v Leaf _)                  = v


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []   = acc           
foldr f acc (x:xs) = f x (foldr f acc xs) 
