--
-- Simple, easy to read implementation of a subset of the Data.Map module.
--
-- The only bits borrowed from the standard lib are the type signatures/names
-- and the original data declaration (minus the strictness)
--
-- @author Julian Fell
--
module Data.Map where

import qualified Data.Maybe as M
import qualified Data.Foldable as F

--
-- Datatypes
--
type Size = Int
data Map k v = Bin Size k v (Map k v) (Map k v) | Tip
  deriving (Show, Eq, Read)

--
-- Constructors
--
empty :: Map k v
empty = Tip

singleton :: (Ord k) => k -> v -> Map k v
singleton k v = Bin 1 k v Tip Tip

--
-- Query Methods
--
null :: Ord k => Map k v -> Bool
null Tip = True
null _ = False

size :: (Ord k, Num a) => Map k v -> a
size Tip = 0
size (Bin s _ _ _ _) = fromIntegral s

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k Tip = Nothing
lookup k (Bin _ a v x y) 
    | a == k    = Just v
    | a < k     = Data.Map.lookup k x
    | otherwise = Data.Map.lookup k y

member :: Ord k => k -> Map k v -> Bool
member k x = M.isJust $ Data.Map.lookup k x

--
-- Insertion Methods
--
insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v Tip = singleton k v
insert k v (Bin s a b x y)
  | k == a = Bin s a v x y
  | k < a  = Bin (s+1) a b x (insert k v y)
  | k > a  = Bin (s+1) a b (insert k v x) y

--
-- Combination Methods
--
union :: Ord k => Map k v -> Map k v -> Map k v
union x y = fromList $ toList x ++ toList y

--
-- List Conversion Methods
--
toList :: Ord k => Map k v -> [(k, v)]
toList Tip = []
toList (Bin _ k v x y) = toList x ++ [(k, v)] ++ toList y

-- Inefficient but simple implementation. Could be improved by sorting then
-- Using the ordering to construct it top-down
fromList :: Ord k => [(k,v)] -> Map k v
fromList [] = Tip
fromList ((k,v):xs) = insert k v (fromList xs)

--
-- Instance Definitions
--
instance Functor (Map k) where
  fmap f Tip = Tip
  fmap f (Bin s k v x y) = Bin s k (f v) (fmap f x) (fmap f y)

-- This is a commutitive monoid because its balanced (I think)
instance (Ord k) => Monoid (Map k v) where
  mempty = empty
  mappend = union

instance Foldable (Map k) where
  foldMap f Tip = mempty
  foldMap f (Bin _ _ v x y) = foldMap f x `mappend` f v `mappend` foldMap f y

