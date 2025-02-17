{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr)

-- * Type definitions

-- | Binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving Show

-- | Forest (i.e. list of 'Tree's)
type Forest a = [Tree a]

-- | Tree traversal order
data Order = PreOrder | InOrder | PostOrder
  deriving Show

-- * Function definitions

-- | Returns values of given 'Tree' in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> torder PreOrder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "A.B.."
-- >>> torder InOrder   (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- ".A.B."
-- >>> torder PostOrder (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "...BA"
--
torder :: Order    -- ^ Order of resulting traversal
       -> Maybe a  -- ^ Optional leaf value
       -> Tree a   -- ^ Tree to traverse
       -> [a]      -- ^ List of values in specified order

torder _ lv Leaf  = maybeToList lv
torder order lv (Branch v l r) = 
  case order of 
    PreOrder   -> v : torder order lv l ++ torder order lv r
    InOrder    -> torder order lv l ++ [v] ++ torder order lv r
    PostOrder  -> torder order lv l ++ torder order lv r ++ [v]

-- | Returns values of given 'Forest' separated by optional separator
-- where each 'Tree' is traversed in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> forder PreOrder  (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|C..|A.B.."
-- >>> forder InOrder   (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|.C.|.A.B."
-- >>> forder PostOrder (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|..C|...BA"
--
forder :: Order     -- ^ Order of tree traversal
       -> Maybe a   -- ^ Optional separator between resulting tree orders
       -> Maybe a   -- ^ Optional leaf value
       -> Forest a  -- ^ List of trees to traverse
       -> [a]       -- ^ List of values in specified tree order

forder _ _ _ [] = []
forder order _ lv [tree] = torder order lv tree
forder order sep lv (tree: rest) =
  let currentTree = [torder order lv tree]  
      restForest  = [forder order sep lv rest]
  in intercalate (maybeToList sep) (currentTree ++ restForest)


maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just v) = [v]

intercalate :: [a] -> [[a]] -> [a]
intercalate sep list = concat (intersperse sep list)

intersperse :: a -> [a] -> [a]
intersperse _ []       = []
intersperse _ [x]      = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

