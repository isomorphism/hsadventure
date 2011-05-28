-----------------------------------------------------------------------------
-- |
-- Module      : Data.FRef
-- Copyright   : (c) 2007 Twan van Laarhoven
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides functional references.
--
-- A functional reference @FRef s a@ provides a way to extract an element of type @a@
-- from structures of type @s@. A new value can be set
--
-----------------------------------------------------------------------------
module Data.FRef
	(	FRef
	,	get, set, setM
	,	update, updateM, updateAll
		-- * From Control.Category
	,	RefArrow(..)
	--	-- * Properties
	--,	prop_get_set
	
	) where

import qualified Prelude as P
import Prelude          (Functor(..), Eq(..), Monad(..), ($), (&&), Bool, Int, Show, Either(..))
import Test.QuickCheck  (property, Arbitrary(..))

import Control.Category
import Control.Category.Defaults
import Control.Category.Properties (Comparable(..))
import Data.Invertible (Invertible(..)) -- for the Arbitrary instance

-----------------------------------------------------------------------------
-- Type : FRef

-- | A functional reference to an @a@ inside an @s@.
--   
--   @FRef@ values can be constructed with @arrRef@ or @arrRef'@.
newtype FRef s a = FRef ( s -> (a, a -> s) )

-- | Get a member from a structure.
get :: FRef s a -> s -> a
get (FRef r) s = P.fst (r s)

-- | Set the value of a member of a structure.
set :: FRef s a -> a -> s -> s
set (FRef r) a s = P.snd (r s) a

-- | Set the value of a member of a structure to all the values from a monadic\/functoric argument
setM :: Functor m => FRef s a -> m a -> (s -> m s)
setM (FRef r) as s = fmap (P.snd $ r s) as

-- | Apply a function to a member of a structure.
update :: FRef s a -> (a -> a) -> (s -> s)
update (FRef r) f s = case r s of (a,u) -> u (f a)

-- | Apply a monadic\/functoric function to a member of a structure.
updateM :: Functor m => FRef s a -> (a -> m a) -> (s -> m s)
updateM (FRef r) f s = let (a,u) = r s in fmap u (f a)

-- | Apply a function to all member of a structure.
--
--   This is a convenience function for references to functor types such as lists.
--   @updateAll@ is defined as simply
--
--   > updateAll r f = update r (fmap f)
updateAll :: Functor f => FRef s (f a) -> (a -> a) -> (s -> s)
updateAll r f = update r (fmap f)

-----------------------------------------------------------------------------
-- Instances: category classes

instance Category FRef where
      id = FRef (\x -> (x, P.id))
      FRef f . FRef g = FRef $ \x -> let (y,u) = g x
                                         (z,v) = f y
                                     in (z, u P.. v)

instance CategoryConst FRef where
      const = const_RefArrow

instance InvArrow FRef where
      arrInv = arrInv_RefArrow

instance RefArrow FRef where
      arrRef = FRef


instance CategoryPair FRef where
      swap = swap_InvArrow
      first  (FRef f)   = FRef $ \ ~(x,y) -> let (a,u) = f x
                                             in ((a,y), \(a',y') -> (u a', y'))
      second (FRef f)   = FRef $ \ ~(x,y) -> let (a,u) = f y
                                             in ((x,a), \(x',a') -> (x', u a'))
      FRef f *** FRef g = FRef $ \ ~(x,y) -> let (a,u) = f x
                                                 (b,v) = g y
                                             in ((a,b), \ ~(a',b') -> (u a', v b'))

instance CategorySelect FRef where
      fst = fst_RefArrow
      snd = snd_RefArrow

instance CategoryFanOut FRef where
      FRef f &&& FRef g = FRef $ \ x -> let (a,_) = f x
                                            (b,v) = g x
                                        in ((a,b), \ ~(a',b') -> let (_,u) = f (v b') in u a')
      dup = FRef $ \ x -> ((x,x), \(x',_) -> x')


-- Functional references do not satisfy the laws of CategoryChoice!!
-- In particular:
--     left id /= id
instance CategoryChoice FRef where
      mirror = mirror_InvArrow
      left  (FRef f) = FRef $ \xy -> case xy of
                                Left  x -> let (a,u) = f x
                                           in (Left a, \ab -> case ab of
                                                   Left  a' -> Left (u a')
                                                   Right b  -> Right b
                                               )
                                Right y -> (Right y, \ab -> case ab of
                                                   Left  _ -> xy
                                                   Right b -> Right b)
      right (FRef f) = FRef $ \xy -> case xy of
                                Right x -> let (a,u) = f x
                                           in (Right a, \ab -> case ab of
                                                   Right a' -> Right (u a')
                                                   Left  b  -> Left b
                                               )
                                Left  y -> (Left y, \ab -> case ab of
                                                   Right _ -> xy
                                                   Left  b -> Left b)

instance CategoryFanIn FRef where
      FRef f ||| FRef g = FRef $ \xy -> case xy of
                           Left  x -> let (a,u) = f x
                                      in (a, Left . u)
                           Right y -> let (a,u) = g y
                                      in (a, Right . u)
      untag = FRef $ \xy -> case xy of
                           Left  x -> (x, Left)
                           Right y -> (y, Right)

instance CategoryInject FRef where
      inl = inl_RefArrow
      inr = inr_RefArrow

-----------------------------------------------------------------------------
-- Instances: QuickCheck

instance (Arbitrary a, Arbitrary b, Eq a, Eq b) => Arbitrary (FRef a b) where
--instance (Arbitrary a, Arbitrary b, Eq a, Eq b, Show a, Show b) => Arbitrary (FRef a b) where
      arbitrary = do
              -- We model (FRef a b) as (Invertible a (b,Junk))
              -- so the structure 'a' contains a value of type 'b' and some other junk
              Invertible f g <- arbitrary
              return $ FRef $ \a -> let (b,junk) = f a
                                    in (b, \b' -> g (b',junk::Int))
      
      coarbitrary (FRef f) = coarbitrary f

instance (Eq a, Eq b, Arbitrary a, Arbitrary b, Show a, Show b) => Comparable (FRef a b) where
      FRef f `equals` FRef g = property $ \x y -> P.fst (f x)   == P.fst (g x)
                                               && P.snd (f x) y == P.snd (g x) y

-- | Test that the given 

-- | Test the generation of FRefs
prop_get_set :: Eq a => FRef a b -> a -> Bool
prop_get_set fr x = set fr (get fr x) x == x

prop_update :: Eq a => FRef a b -> a -> Bool
prop_update fr x = update fr id x == x
