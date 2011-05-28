{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Control.Category.Internal.Comparable
-- Copyright   : (c) 2007 Twan van Laarhoven
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : non-portable (flexible instances)
-- 
-- Comparable type class for quick check tests
--
-----------------------------------------------------------------------------
module Control.Category.Internal.Comparable
        ( Comparable(..)
        ) where

import Test.QuickCheck
import qualified Prelude as P
import Prelude          ( Eq(..), Show(..)
                        , ($)
                        , length, zipWith, (!!)
                        , mod
                        , Int, Bool(..), Char, Either
                        )

-----------------------------------------------------------------------------

-- | Testing values for equality.
class Arbitrary a => Comparable a where
      equals :: a -> a -> Property

infix 1 `equals`

-----------------------------------------------------------------------------
-- Instances

instance Testable a => Testable [a] where
      property [] = property True
      property props = property $ \n -> props !! (n `mod` length props)

instance Comparable Int  where equals a b = property $ a == b
instance Comparable Bool where equals a b = property $ a == b
instance (Eq a, Arbitrary a) => Comparable (a,a) where
                               equals a b = property $ a == b
instance (Eq a, Arbitrary a) => Comparable (Either a a) where
                               equals a b = property $ a == b

instance Comparable a => Comparable [a] where
      xs `equals` ys = property $ (property $ length xs == length ys)
                                : zipWith equals xs ys

instance (Arbitrary a, Show a, Comparable b) => Comparable (a -> b) where
      equals f g = property $ \x -> f x `equals` g x
