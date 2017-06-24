{-# LANGUAGE GADTSyntax           #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

import Test.QuickCheck
import GHC.Generics
import Generic.Random.Generic

import Data.Map

instance Arbitrary a => Arbitrary (Map k a) where
  arbitrary = genericArbitrary

-- Just want to get set up, so this is a simple first prop to test
-- Eventually, I want to validate the functor laws
prop_size :: (Ord k) => k -> a -> Map k a -> Bool
prop_size k v map = size (insert k v map) >= size map

main = sample (arbitrary :: Gen (Map Int Int))
