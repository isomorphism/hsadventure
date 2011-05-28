-----------------------------------------------------------------------------
-- |
-- Module      : Data.Invertible
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides invertible functions, or /bijections/.
--
-----------------------------------------------------------------------------
module Data.Invertible
        ( Invertible(..)
        , call, inverse
        ) where

import qualified Prelude as P
import Prelude          (Eq(..), Monad(..), ($), (&&), repeat, sequence, lookup, map, filter, Maybe(..), Bool, Show)
import Test.QuickCheck  (property, Arbitrary(..))
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef       (newIORef, readIORef, writeIORef)
import Control.Exception(evaluate)

import Control.Category
import Control.Category.Defaults
import Control.Category.Internal.Comparable (Comparable(..))

-----------------------------------------------------------------------------
-- Type

-- | A function with an inverse
data Invertible a b = Invertible (a -> b) (b -> a)

-- | Use an invertible function
call :: InvArrow cat => Invertible a b -> cat a b
call (Invertible f g) = arrInv f g

-- | The inverse of an 'Invertible' function.
inverse :: InvArrow cat => Invertible a b -> cat b a
inverse (Invertible f g) = arrInv g f

-----------------------------------------------------------------------------
-- Instances: category classes

instance Category Invertible where
      id = Invertible id id
      Invertible f g . Invertible h i = Invertible (f . h) (i . g)

instance InvArrow Invertible where
      arrInv = Invertible

instance CategoryPair Invertible where
      swap = swap_InvArrow
      first  (Invertible f g) = Invertible (first  f) (first  g)
      second (Invertible f g) = Invertible (second f) (second g)
      Invertible f g *** Invertible h i = Invertible (f *** h) (g *** i)

instance CategoryChoice Invertible where
      mirror = mirror_InvArrow
      left  (Invertible f g) = Invertible (left  f) (left  g)
      right (Invertible f g) = Invertible (right f) (right g)
      Invertible f g +++ Invertible h i = Invertible (f +++ h) (g +++ i)

instance CategoryLoop Invertible where
      loop (Invertible f g) = Invertible (loop f) (loop g)

-----------------------------------------------------------------------------
-- Instances: QuickCheck

{-
-- This should perhaps be moved to Test.QuickCheck.Function

-- | An invertible function, with a table of used values
data InvertibleFunction a b = InvertibleFunction (FunctionTable a b) (a -> b) (b -> a)
-}

instance (Arbitrary a, Arbitrary b, Eq a, Eq b) => Arbitrary (Invertible a b) where
--instance (Arbitrary a, Arbitrary b, Eq a, Eq b, Show a, Show b) => Arbitrary (Invertible a b) where
      arbitrary = do
            -- We need to generate a function and its inverse.
            -- One way to do this is to keep a map of all the values used,
            --  while ensuring there are no duplicates
            -- We are going to need both arbitrary as and bs
            initialAs <- sequence (repeat arbitrary)
            initialBs <- sequence (repeat arbitrary)
            -- Magic in the IO Monad: 
            let {-# NOINLINE ref #-}
                ref = unsafePerformIO $ newIORef ([], initialAs, initialBs)
                -- Add a to the table if it is not already there
                addA a = do  evaluate (a==a) -- fully force a
                             (assocs, as, bs) <- readIORef ref
                             case lookup a assocs of
                               Just b  -> return b
                               Nothing -> do
                                 let (b:bs') = bs
                                 --P.putStrLn $ P.show a P.++ " -> " P.++ P.show b
                                 writeIORef ref ((a,b):assocs, filter (/=a) as, filter (/=b) bs')
                                 return b
                addB b = do  evaluate (b==b)
                             (assocs, as, bs) <- readIORef ref
                             case lookup b (map swap assocs) of
                               Just a  -> return a
                               Nothing -> do
                                 let (a:as') = as
                                 --P.putStrLn $ P.show a P.++ " -> " P.++ P.show b
                                 writeIORef ref ((a,b):assocs, filter (/=a) as', filter (/=b) bs)
                                 return a
            -- Now make our function
            return $ Invertible (unsafePerformIO . addA) (unsafePerformIO . addB)
      
      coarbitrary (Invertible f g) = coarbitrary f . coarbitrary g

instance (Eq a, Eq b, Arbitrary a, Arbitrary b, Show a, Show b) => Comparable (Invertible a b) where
      Invertible f g `equals` Invertible h i
         = property $ \x y -> f x == h x && g y == i y

{-
-- | Test that the given Invertible function indeed has a proper inverse
prop_invertible :: (Arbitrary a, Arbitrary b, Eq a, Eq b) => Invertible a a -> Property
prop_invertible f = f . inverse f `equals` id

-- | Test the generation of invertible functions
prop_invertible :: Eq a => Invertible a a -> a -> Bool
prop_invertible (Invertible f g) x = (f . g) x == x && (g . f) x == x
-}